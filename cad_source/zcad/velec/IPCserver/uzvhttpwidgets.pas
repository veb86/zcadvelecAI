{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************

@author(HTTP Widgets Server for ZCAD)
@author(Vladimir Bobrov)

Раздача локальных файлов Grist Widgets через HTTP-сервер ZCAD.

Все файлы:

  C:\zcad\GristWidgets\

доступны через:

  http://127.0.0.1:5000/widgets/

Например:

  C:\zcad\GristWidgets\managerCalc\index.html

доступен как:

  http://127.0.0.1:5000/widgets/managerCalc/index.html
}

{$mode objfpc}{$H+}

unit uzvhttpwidgets;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  httpdefs,
  uzclog,
  fphttpserver,
  uzbLogTypes;

const

  {** Корневой каталог Grist Widgets }
  HTTP_WIDGETS_PATH = 'C:\zcad\GristWidgets\widget';

  {** HTTP-префикс для виджетов }
  HTTP_WIDGETS_PREFIX = '/widgets';


type

  {**
    Сервер статических файлов Grist Widgets.

    Сам HTTP-сервер находится в uzvhttpserver.pas.

    Этот класс только обрабатывает HTTP-запрос,
    определяет файл внутри FRootPath и возвращает его клиенту.
  }
  TZCADHTTPWidgets = class
  private

    FRootPath: string;

    procedure Log(
      const AMessage: string;
      ALogLevel: TLogLevel
    );

    function GetContentType(
      const AFileName: string
    ): string;

    function GetFilePath(
      const AURI: string
    ): string;

    function GetRelativePath(
      const AURI: string
    ): string;

    function DirectoryListing(
      const ADirectory: string;
      const AURI: string
    ): string;

    function FileExistsSafe(
      const AFileName: string
    ): Boolean;

    function DirectoryExistsSafe(
      const ADirectory: string
    ): Boolean;

    procedure SendText(
      var AResponse: TFPHTTPConnectionResponse;
      const AText: string;
      const AContentType: string;
      ACode: Integer = 200
    );

    procedure SendFile(
      var AResponse: TFPHTTPConnectionResponse;
      const AFileName: string
    );

  public

    constructor Create(
      const ARootPath: string = HTTP_WIDGETS_PATH
    );

    procedure HandleRequest(
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse
    );

    property RootPath: string read FRootPath write FRootPath;

  end;


var

  {** Глобальный обработчик Widgets }
  ZCADHTTPWidgets: TZCADHTTPWidgets;


{** Инициализация Widgets HTTP-модуля }
procedure HTTPWidgetsInit;

{** Завершение Widgets HTTP-модуля }
procedure HTTPWidgetsDone;

{** Обработать HTTP-запрос Widgets }
procedure HTTPWidgetsHandleRequest(
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);


implementation


{=============================================================================
  TZCADHTTPWidgets
=============================================================================}

constructor TZCADHTTPWidgets.Create(
  const ARootPath: string
);
begin

  inherited Create;

  FRootPath := ExcludeTrailingPathDelimiter(
    ExpandFileName(ARootPath)
  );

end;


{=============================================================================
  LOG
=============================================================================}

procedure TZCADHTTPWidgets.Log(
  const AMessage: string;
  ALogLevel: TLogLevel
);
begin

  ProgramLog.LogOutFormatStr(
    '[HTTP-Widgets] %s',
    [AMessage],
    ALogLevel,
    0
  );

end;


{=============================================================================
  CONTENT TYPE
=============================================================================}

function TZCADHTTPWidgets.GetContentType(
  const AFileName: string
): string;
var
  Ext: string;
begin

  Ext := LowerCase(
    ExtractFileExt(AFileName)
  );


  if Ext = '.html' then
    Exit('text/html; charset=utf-8');

  if Ext = '.htm' then
    Exit('text/html; charset=utf-8');

  if Ext = '.js' then
    Exit('application/javascript; charset=utf-8');

  if Ext = '.mjs' then
    Exit('application/javascript; charset=utf-8');

  if Ext = '.css' then
    Exit('text/css; charset=utf-8');

  if Ext = '.json' then
    Exit('application/json; charset=utf-8');

  if Ext = '.txt' then
    Exit('text/plain; charset=utf-8');

  if Ext = '.xml' then
    Exit('application/xml; charset=utf-8');

  if Ext = '.svg' then
    Exit('image/svg+xml');

  if Ext = '.png' then
    Exit('image/png');

  if Ext = '.jpg' then
    Exit('image/jpeg');

  if Ext = '.jpeg' then
    Exit('image/jpeg');

  if Ext = '.gif' then
    Exit('image/gif');

  if Ext = '.webp' then
    Exit('image/webp');

  if Ext = '.ico' then
    Exit('image/x-icon');

  if Ext = '.bmp' then
    Exit('image/bmp');

  if Ext = '.pdf' then
    Exit('application/pdf');

  if Ext = '.zip' then
    Exit('application/zip');

  if Ext = '.csv' then
    Exit('text/csv; charset=utf-8');

  if Ext = '.woff' then
    Exit('font/woff');

  if Ext = '.woff2' then
    Exit('font/woff2');

  if Ext = '.ttf' then
    Exit('font/ttf');

  if Ext = '.otf' then
    Exit('font/otf');

  if Ext = '.eot' then
    Exit('application/vnd.ms-fontobject');


  {---------------------------------------------------------------------------
    Неизвестный тип.

    Для Grist Widget это лучше отдавать как бинарный поток.
  ---------------------------------------------------------------------------}

  Result := 'application/octet-stream';

end;


{=============================================================================
  GET RELATIVE PATH
=============================================================================}

function TZCADHTTPWidgets.GetRelativePath(
  const AURI: string
): string;
begin

  Result := AURI;


  {---------------------------------------------------------------------------
    Убираем query string.

    Например:

      /widgets/index.html?test=1

    превращается в:

      /widgets/index.html
  ---------------------------------------------------------------------------}

  if Pos('?', Result) > 0 then
    Result :=
      Copy(
        Result,
        1,
        Pos('?', Result) - 1
      );


  {---------------------------------------------------------------------------
    Убираем начало /widgets
  ---------------------------------------------------------------------------}

  if Pos(
       LowerCase(HTTP_WIDGETS_PREFIX),
       LowerCase(Result)
     ) = 1 then
  begin

    Delete(
      Result,
      1,
      Length(HTTP_WIDGETS_PREFIX)
    );

  end;


  {---------------------------------------------------------------------------
    Начальный / нам не нужен для построения пути.
  ---------------------------------------------------------------------------}

  while
    (Length(Result) > 0) and
    ((Result[1] = '/') or (Result[1] = '\')) do
  begin

    Delete(Result, 1, 1);

  end;

end;


{=============================================================================
  GET FILE PATH
=============================================================================}

function TZCADHTTPWidgets.GetFilePath(
  const AURI: string
): string;
var
  RelativePath: string;
begin

  RelativePath := GetRelativePath(AURI);


  if RelativePath = '' then
  begin

    Result := FRootPath;
    Exit;

  end;


  {---------------------------------------------------------------------------
    HTTP использует /.

    Windows использует \.

    ConvertPathDelims приводит путь к системному виду.
  ---------------------------------------------------------------------------}

  RelativePath :=
    StringReplace(
      RelativePath,
      '/',
      PathDelim,
      [rfReplaceAll]
    );


  Result :=
    IncludeTrailingPathDelimiter(FRootPath) +
    RelativePath;

end;


{=============================================================================
  FILE EXISTS
=============================================================================}

function TZCADHTTPWidgets.FileExistsSafe(
  const AFileName: string
): Boolean;
begin

  try

    Result :=
      FileExists(AFileName);

  except

    Result := False;

  end;

end;


{=============================================================================
  DIRECTORY EXISTS
=============================================================================}

function TZCADHTTPWidgets.DirectoryExistsSafe(
  const ADirectory: string
): Boolean;
begin

  try

    Result :=
      DirectoryExists(ADirectory);

  except

    Result := False;

  end;

end;


{=============================================================================
  SEND TEXT
=============================================================================}

procedure TZCADHTTPWidgets.SendText(
  var AResponse: TFPHTTPConnectionResponse;
  const AText: string;
  const AContentType: string;
  ACode: Integer
);
begin

  AResponse.Code := ACode;

  AResponse.ContentType :=
    AContentType;

  AResponse.Content :=
    AText;

end;


{=============================================================================
  SEND FILE
=============================================================================}

procedure TZCADHTTPWidgets.SendFile(
  var AResponse: TFPHTTPConnectionResponse;
  const AFileName: string
);
var
  Stream: TFileStream;
begin

  Stream := nil;

  try

    Stream :=
      TFileStream.Create(
        AFileName,
        fmOpenRead or fmShareDenyNone
      );


    AResponse.Code := 200;

    AResponse.ContentType :=
      GetContentType(AFileName);

    AResponse.ContentLength :=
      Stream.Size;

    {-----------------------------------------------------------------------
      Передаём файл как поток.

      Это важно для SVG, PNG, JPG, шрифтов и других
      бинарных файлов.
    -----------------------------------------------------------------------}

    AResponse.ContentStream :=
      Stream;

    Stream := nil;


    Log(
      Format(
        'File served: %s (%d bytes)',
        [
          AFileName,
          AResponse.ContentLength
        ]
      ),
      LM_Debug
    );

  except

    on E: Exception do
    begin

      if Stream <> nil then
        Stream.Free;


      Log(
        Format(
          'Error reading file "%s": %s',
          [
            AFileName,
            E.Message
          ]
        ),
        LM_Error
      );


      AResponse.Code := 500;

      AResponse.ContentType :=
        'text/plain; charset=utf-8';

      AResponse.Content :=
        'Internal server error';

    end;

  end;

end;


{=============================================================================
  DIRECTORY LISTING
=============================================================================}

function TZCADHTTPWidgets.DirectoryListing(
  const ADirectory: string;
  const AURI: string
): string;
var
  SR: TSearchRec;
  FindResult: Integer;
  Name: string;
  FullName: string;
  Href: string;
  RelativeURI: string;
begin

  Result :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '<meta charset="utf-8">' +
    '<title>ZCAD Grist Widgets</title>' +
    '<style>' +
    'body {' +
    'font-family: Arial, sans-serif;' +
    'margin: 30px;' +
    '}' +
    'a {' +
    'text-decoration: none;' +
    '}' +
    'li {' +
    'margin: 6px 0;' +
    '}' +
    '</style>' +
    '</head>' +
    '<body>';


  Result :=
    Result +
    '<h1>ZCAD Grist Widgets</h1>';


  Result :=
    Result +
    '<p>' +
    'Root: ' +
    FRootPath +
    '</p>';


  {---------------------------------------------------------------------------
    Ссылка на родительский каталог
  ---------------------------------------------------------------------------}

  RelativeURI := AURI;

  while
    (Length(RelativeURI) > 0) and
    (RelativeURI[Length(RelativeURI)] = '/') do
  begin
    Delete(
      RelativeURI,
      Length(RelativeURI),
      1
    );
  end;


  if Pos(
       HTTP_WIDGETS_PREFIX,
       RelativeURI
     ) = 1 then
  begin

    if RelativeURI <> HTTP_WIDGETS_PREFIX then
    begin

      RelativeURI :=
        ExtractFileDir(RelativeURI);

      if RelativeURI = '.' then
        RelativeURI :=
          HTTP_WIDGETS_PREFIX;

    end;

  end;


  if AURI <> HTTP_WIDGETS_PREFIX + '/' then
  begin

    Result :=
      Result +
      '<p><a href="' +
      HTTP_WIDGETS_PREFIX +
      '/">[ Widgets root ]</a></p>';

  end;


  Result :=
    Result +
    '<ul>';


  {---------------------------------------------------------------------------
    Читаем каталог
  ---------------------------------------------------------------------------}

  FindResult :=
    FindFirst(
      IncludeTrailingPathDelimiter(ADirectory) + '*',
      faAnyFile,
      SR
    );


  if FindResult = 0 then
  begin

    try

      repeat

        Name := SR.Name;


        if
          (Name = '.') or
          (Name = '..')
        then
          Continue;


        FullName :=
          IncludeTrailingPathDelimiter(ADirectory) +
          Name;


        {---------------------------------------------------------------------
          Каталог
        ---------------------------------------------------------------------}

        if (SR.Attr and faDirectory) <> 0 then
        begin

          Href :=
            AURI;

          if Href[Length(Href)] <> '/' then
            Href := Href + '/';

          Href :=
            Href + Name + '/';


          Result :=
            Result +
            '<li>📁 <a href="' +
            Href +
            '">' +
            Name +
            '/</a></li>';

        end

        {---------------------------------------------------------------------
          Файл
        ---------------------------------------------------------------------}

        else
        begin

          Href :=
            AURI;

          if Href[Length(Href)] <> '/' then
            Href := Href + '/';

          Href :=
            Href + Name;


          Result :=
            Result +
            '<li>📄 <a href="' +
            Href +
            '">' +
            Name +
            '</a></li>';

        end;


      until FindNext(SR) <> 0;

    finally

      FindClose(SR);

    end;

  end;


  Result :=
    Result +
    '</ul>';


  Result :=
    Result +
    '</body>' +
    '</html>';

end;


{=============================================================================
  HANDLE REQUEST
=============================================================================}

procedure TZCADHTTPWidgets.HandleRequest(
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);
var
  FilePath: string;
  URI: string;
begin

  URI := ARequest.URI;


  Log(
    Format(
      'Widgets request: %s %s',
      [
        ARequest.Method,
        URI
      ]
    ),
    LM_Debug
  );


  {---------------------------------------------------------------------------
    Разрешаем GET и HEAD.

    HEAD полезен браузерам и некоторым web-инструментам.
  ---------------------------------------------------------------------------}

  if
    (UpperCase(ARequest.Method) <> 'GET') and
    (UpperCase(ARequest.Method) <> 'HEAD')
  then
  begin

    AResponse.Code := 405;

    AResponse.ContentType :=
      'application/json';

    AResponse.Content :=
      '{"status":"error","error":"Method not allowed"}';

    Exit;

  end;


  {---------------------------------------------------------------------------
    Получаем реальный путь.
  ---------------------------------------------------------------------------}

  FilePath :=
    GetFilePath(URI);


  Log(
    Format(
      'Widgets file path: %s',
      [FilePath]
    ),
    LM_Debug
  );


  {---------------------------------------------------------------------------
    Если это каталог.
  ---------------------------------------------------------------------------}

  if DirectoryExistsSafe(FilePath) then
  begin

    {-----------------------------------------------------------------------
      Если в каталоге есть index.html, отдаём его автоматически.
    -----------------------------------------------------------------------}

    if FileExistsSafe(
         IncludeTrailingPathDelimiter(FilePath) +
         'index.html'
       )
    then
    begin

      if UpperCase(ARequest.Method) = 'HEAD' then
      begin

        AResponse.Code := 200;

        AResponse.ContentType :=
          'text/html; charset=utf-8';

        Exit;

      end;


      SendFile(
        AResponse,
        IncludeTrailingPathDelimiter(FilePath) +
        'index.html'
      );

      Exit;

    end;


    {-----------------------------------------------------------------------
      Иначе показываем содержимое каталога.
    -----------------------------------------------------------------------}

    if UpperCase(ARequest.Method) = 'HEAD' then
    begin

      AResponse.Code := 200;

      AResponse.ContentType :=
        'text/html; charset=utf-8';

      Exit;

    end;


    SendText(
      AResponse,
      DirectoryListing(
        FilePath,
        URI
      ),
      'text/html; charset=utf-8',
      200
    );

    Exit;

  end;


  {---------------------------------------------------------------------------
    Если это файл.
  ---------------------------------------------------------------------------}

  if FileExistsSafe(FilePath) then
  begin

    if UpperCase(ARequest.Method) = 'HEAD' then
    begin

      AResponse.Code := 200;

      AResponse.ContentType :=
        GetContentType(FilePath);

      Exit;

    end;


    SendFile(
      AResponse,
      FilePath
    );

    Exit;

  end;


  {---------------------------------------------------------------------------
    Файл или каталог не найден.
  ---------------------------------------------------------------------------}

  AResponse.Code := 404;

  AResponse.ContentType :=
    'text/html; charset=utf-8';

  AResponse.Content :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '<meta charset="utf-8">' +
    '<title>404 Not Found</title>' +
    '</head>' +
    '<body>' +
    '<h1>404 - Not Found</h1>' +
    '<p>Widget resource not found.</p>' +
    '<p>' +
    URI +
    '</p>' +
    '</body>' +
    '</html>';


  Log(
    Format(
      'Widget resource not found: %s',
      [FilePath]
    ),
    LM_Info
  );

end;


{=============================================================================
  GLOBAL FUNCTIONS
=============================================================================}

procedure HTTPWidgetsInit;
begin

  if ZCADHTTPWidgets <> nil then
    Exit;


  ZCADHTTPWidgets :=
    TZCADHTTPWidgets.Create(
      HTTP_WIDGETS_PATH
    );


  ProgramLog.LogOutFormatStr(
    'HTTP Widgets module initialized: %s',
    [
      HTTP_WIDGETS_PATH
    ],
    LM_Info,
    0
  );

end;


procedure HTTPWidgetsDone;
begin

  if ZCADHTTPWidgets <> nil then
  begin

    ZCADHTTPWidgets.Free;

    ZCADHTTPWidgets := nil;


    ProgramLog.LogOutFormatStr(
      'HTTP Widgets module finalized',
      [],
      LM_Info,
      0
    );

  end;

end;


procedure HTTPWidgetsHandleRequest(
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse
);
begin

  if ZCADHTTPWidgets = nil then
    HTTPWidgetsInit;


  ZCADHTTPWidgets.HandleRequest(
    ARequest,
    AResponse
  );

end;


{=============================================================================
  INITIALIZATION / FINALIZATION
=============================================================================}

initialization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsInitializeLMId
  );


  HTTPWidgetsInit;


finalization

  HTTPWidgetsDone;


  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.
