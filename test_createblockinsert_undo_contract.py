from pathlib import Path
import unittest


REPO_ROOT = Path(__file__).resolve().parent
CREATE_BLOCK_INSERT = REPO_ROOT / "cad_source/zcad/velec/createBlock/uzccommand_createblockinsert.pas"
INSERT_COMMAND = REPO_ROOT / "cad_source/zcad/commands/uzccommand_insert.pas"


def read_source(path: Path) -> str:
    return path.read_text(encoding="utf-8")


class CreateBlockInsertUndoContractTest(unittest.TestCase):
    def test_createblockinsert_registers_blockdef_undo_in_marker(self):
        source = read_source(CREATE_BLOCK_INSERT)

        self.assertIn("TCreateBlockDefUndoCommand=class(TUCmdBase)", source)
        self.assertIn(".UndoStack.PushStartMarker(CommandName)", source)
        self.assertIn("PushCreateBlockDefUndoCommand(pBlockDef)", source)
        self.assertIn(".UndoStack.PushEndMarker", source)

    def test_createblockinsert_closes_marker_from_insert_callbacks(self):
        source = read_source(CREATE_BLOCK_INSERT)

        self.assertIn("SetInsertOneShotCallbacks(", source)
        self.assertIn("@FinishCreateBlockInsertUndo", source)
        self.assertIn("@CancelCreateBlockInsertUndo", source)

    def test_insert_exposes_one_shot_callbacks_for_wrapping_commands(self):
        source = read_source(INSERT_COMMAND)

        self.assertIn("TInsertCommandEndProc=procedure", source)
        self.assertIn("SetInsertOneShotCallbacks", source)
        self.assertIn("OneShotAfterInsertProc", source)
        self.assertIn("OneShotInsertCommandEndProc", source)
        self.assertIn("ClearInsertOneShotCallbacks", source)


if __name__ == "__main__":
    unittest.main()
