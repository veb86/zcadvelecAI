"""Bridge ZCAD HTTP IPC to an external classifier that reads and writes JSON."""

import argparse
import json
import math
import os
import subprocess
import time
import uuid
from urllib.request import Request, urlopen


class IPCError(RuntimeError):
    pass


def ipc(endpoint, token, command, *arguments):
    payload = {"id": uuid.uuid4().hex, "cmd": command, "args": arguments}
    if token:
        payload["token"] = token
    request = Request(endpoint, data=json.dumps(payload).encode(),
                      headers={"Content-Type": "application/json"})
    with urlopen(request, timeout=35) as stream:
        response = json.load(stream)
    if response.get("id") != payload["id"]:
        raise IPCError("Unexpected IPC response ID")
    if response.get("status") != "ok":
        raise IPCError(response.get("error", "ZCAD IPC failed"))
    return json.loads(response["result"]) if "result" in response else None


def recognize(call, session, classifier, timeout):
    candidates = call("WALL_SEND_AI", session)
    try:
        process = subprocess.run(
            classifier, input=json.dumps(candidates), text=True, encoding="utf-8",
            stdout=subprocess.PIPE, check=True, timeout=timeout,
        )
        return call("WALL_AI_RESULT", json.loads(process.stdout))
    except (OSError, ValueError, subprocess.SubprocessError, IPCError):
        try:
            call("WALL_CANCEL", session)
        except (OSError, ValueError, IPCError):
            pass
        raise


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--endpoint", default="http://127.0.0.1:5000/ipc")
    parser.add_argument("--timeout", type=float, default=120, help="Classifier timeout in seconds")
    parser.add_argument("classifier", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    classifier = args.classifier
    if classifier[:1] == ["--"]:
        classifier = classifier[1:]
    if not classifier or not math.isfinite(args.timeout) or args.timeout <= 0:
        parser.error("Provide a classifier command after -- and a positive timeout")
    token = os.environ.get("ZCAD_IPC_TOKEN", "")

    def call(command, *arguments):
        return ipc(args.endpoint, token, command, *arguments)

    try:
        status = call("WALL_PREVIEW")
        if status["phase"] == "idle":
            raise IPCError("Start WallAnalyze in ZCAD first")
        session = status["session_id"]
        if status["phase"] == "candidate_preview":
            print("Review the candidates and enter WallSend or WallCancel in ZCAD.", flush=True)
        while status["phase"] == "candidate_preview":
            time.sleep(1)
            status = call("WALL_PREVIEW", session)
        if status["phase"] in ("cancelled", "invalidated"):
            return
        if status["phase"] not in ("approved_to_send", "awaiting_ai"):
            raise IPCError(f"Unexpected session phase {status['phase']}")
        status = recognize(call, session, classifier, args.timeout)
        print(json.dumps(status, indent=2, ensure_ascii=False))
        print("Review the result and enter WallCommit or WallCancel in ZCAD.")
    except (ValueError, OSError, subprocess.SubprocessError, IPCError) as error:
        parser.exit(1, f"{error}\n")


if __name__ == "__main__":
    main()
