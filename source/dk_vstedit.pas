unit DK_VSTEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, VirtualTrees, LCLType, Spin,
  LMessages, LCLIntf, DateTimePicker, BCButton, Dialogs,

  DK_VSTTypes, DK_VSTCore, DK_Const, DK_Vector, DK_Matrix, DK_StrUtils,
  DK_VSTDropDown;

type

  { TVSTEdit }

  TVSTEdit = class (TVSTCustomSimpleTable)
  protected
    FPicks: TStrMatrix;
    FKeys: TIntMatrix;
    FCellTextBeforeEditing: String;
    FColumnTypes: TVSTColumnTypes;
    FColumnFormatStrings: TStrVector;
    FDecimalPlaces: TIntVector;
    FSelectedRowIndex, FSelectedColIndex: Integer;
    FTitleColumnIndex: Integer;

    FIsShowZeros: Boolean;
    FIsUnselectOnExit: Boolean;
    FIsEditing: Boolean;
    FIsOneRowEditing: Boolean;
    FIsBeginEditOnKeyPress: Boolean;

    FEditor: TControl;
    FDropDown: TDropDown;
    FOnEdititingDone: TVSTEdititingDoneEvent;
    FOnEdititingBegin: TVSTEvent;
    FColumnRowTitlesFont: TFont;
    //FColumnRowTitlesBGColor: TColor;
    FColorColumnBorderColor: TColor;
    FColorColumnCellMargin: Integer;

    procedure SelectCell(Node: PVirtualNode; Column: TColumnIndex; const ASaveChanges: Boolean = True);
    procedure UnselectCell(const ASaveChanges: Boolean = True);

    procedure HeaderClear; override;

    function IsColumnValuesExists(const AColIndex: Integer): Boolean;

    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    function GetIsSelected: Boolean;
    function GetSelectedText: String;
    procedure SetSelectedText(AValue: String);
    function SelectedCellRect: TRect;


    procedure SetShowZeros(AValue: Boolean);

    function IsEditingColIndexCorrect(const AIndex: Integer): Boolean;
    function IsRowIndexCorrect(const AIndex: Integer): Boolean;

    function GetColumnRowTitlesBGColor: TColor;
    procedure SetColumnRowTitlesBGColor(AValue: TColor);
    procedure SetColumnRowTitlesFont(AValue: TFont);
    procedure SetColumnRowTitlesVisible(AValue: Boolean);

    procedure SetNewColumnSettings(const AColumnType: TVSTColumnType;
                                   const AFormatString: String;
                                   const ADecimalPlaces: Integer = 0);
    procedure AddValuesColumn(const AColumnType: TVSTColumnType;
                        const ACaption, AFormatString: String;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter;
                        const AKeys: TIntVector = nil;
                        const APicks: TStrVector = nil;
                        const ADecimalPlaces: Integer = 0);

    function GetIsRowTitlesColumnExists: Boolean;

    function GetRowValues(const ARowIndex: Integer): TStrVector;
    procedure SetRowValues(const ARowIndex: Integer; const ARowValues: TStrVector);

    function CellRectangle(Column: TColumnIndex; ACellRect: TRect): TRect; override;
    function CellGridColor(Node: PVirtualNode; {%H-}Column: TColumnIndex; ABGColor: TColor): TColor; override;
    function CellBGColor(Node: PVirtualNode; {%H-}Column: TColumnIndex): TColor; override;
    procedure CellFont(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DeleteSelectedCellText;

    procedure MoveSelectionVertical(const ADirection {1 down, -1 up}: Integer);
    procedure MoveSelectionHorizontal(const ADirection {1 right, -1 left}: Integer);

    procedure BeginEdit;
    procedure EndEdit(const ASaveChanges: Boolean = True);

    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure KeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure EditorKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure EditorExit;

    procedure TreeExit(Sender: TObject);
    procedure TreeColumnResize(Sender: TVTHeader; {%H-}Column: TColumnIndex);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor  Destroy; override;

    procedure EditingDone;

    procedure ValuesClear; override;
    procedure Draw; override;

    procedure AutosizeColumnRowTitlesEnable;

    procedure AddColumnRowTitles(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter);
    procedure SetColumnRowTitles(const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnInteger(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnDouble(const ACaption: String;
                        const ADecimalPlaces: Integer = 2;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnString(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnDate(const ACaption, AFormatString: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnTime(const ACaption, AFormatString: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnKeyPick(const ACaption: String;
                        const AKeys: TIntVector; const APicks: TStrVector;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnColor(const ACaption: String; const AWidth: Integer = 100;
                             const ACaptionAlignment: TAlignment = taCenter);

    procedure SetColumnInteger(const ACaption: String; const AValues: TIntVector);
    procedure SetColumnDouble(const ACaption: String; const AValues: TDblVector);
    procedure SetColumnString(const ACaption: String; const AValues: TStrVector);
    procedure SetColumnDate(const ACaption: String; const AValues: TDateVector);
    procedure SetColumnTime(const ACaption: String; const AValues: TTimeVector);
    procedure SetColumnColor(const ACaption: String; const AValues: TColorVector);

    procedure SetColumnInteger(const AColIndex: Integer; const AValues: TIntVector);
    procedure SetColumnDouble(const AColIndex: Integer; const AValues: TDblVector);
    procedure SetColumnString(const AColIndex: Integer; const AValues: TStrVector);
    procedure SetColumnDate(const AColIndex: Integer; const AValues: TDateVector);
    procedure SetColumnTime(const AColIndex: Integer; const AValues: TTimeVector);
    procedure SetColumnColor(const AColIndex: Integer; const AValues: TColorVector);

    procedure UnSelect(const ASaveChanges: Boolean = True);
    procedure Select(const ARowIndex, AColIndex: Integer);
    procedure Select(const ARowIndex: Integer; const AColumnCaption: String);
    procedure Select(const ARowTitle, AColumnCaption: String);

    procedure Show(const ARowIndex: Integer);

    function ColumnAsInteger(out AValues: TIntVector; const AColIndex: Integer;
                             const ADefaultValue: Integer = 0): Boolean;
    function ColumnAsDouble(out AValues: TDblVector; const AColIndex: Integer;
                             const ADefaultValue: Integer = 0): Boolean;
    function ColumnAsString(out AValues: TStrVector; const AColIndex: Integer;
                             const ADefaultValue: String = ''): Boolean;
    function ColumnAsDate(out AValues: TDblVector; const AColIndex: Integer;
                          const ADefaultValue: TDate = 0): Boolean;
    function ColumnAsTime(out AValues: TDblVector; const AColIndex: Integer;
                          const ADefaultValue: TTime = 0): Boolean;
    function ColumnAsColor(out AValues: TColorVector; const AColIndex: Integer;
                          const ADefaultValue: TColor = clBlack): Boolean;

    function ColumnAsInteger(out AValues: TIntVector; const ACaption: String;
                             const ADefaultValue: Integer = 0): Boolean;
    function ColumnAsDouble(out AValues: TDblVector; const ACaption: String;
                             const ADefaultValue: Integer = 0): Boolean;
    function ColumnAsString(out AValues: TStrVector; const ACaption: String;
                             const ADefaultValue: String = ''): Boolean;
    function ColumnAsDate(out AValues: TDblVector; const ACaption: String;
                          const ADefaultValue: TDate = 0): Boolean;
    function ColumnAsTime(out AValues: TDblVector; const ACaption: String;
                          const ADefaultValue: TTime = 0): Boolean;
    function ColumnAsColor(out AValues: TColorVector; const ACaption: String;
                          const ADefaultValue: TColor = clBlack): Boolean;

    procedure RowInsert(const ARowIndex: Integer; const AValues: TStrVector = nil);
    procedure RowDelete(const ARowIndex: Integer);
    property RowValues[ARowIndex: Integer]: TStrVector read GetRowValues write SetRowValues;

    procedure SetColumnRowTitlesHeaderBGColor(const ABGColor: TColor);
    property ColumnRowTitlesFont: TFont read FColumnRowTitlesFont write SetColumnRowTitlesFont;
    property ColumnRowTitlesBGColor: TColor read GetColumnRowTitlesBGColor write SetColumnRowTitlesBGColor;

    property IsColumnRowTitlesExists: Boolean read GetIsRowTitlesColumnExists;
    property ColumnRowTitlesVisible: Boolean write SetColumnRowTitlesVisible;

    property IsBeginEditOnKeyPress: Boolean read FIsBeginEditOnKeyPress write FIsBeginEditOnKeyPress;
    property IsOneRowEditing: Boolean read FIsOneRowEditing write FIsOneRowEditing;
    property IsEditing: Boolean read FIsEditing;
    property IsSelected: Boolean read GetIsSelected;
    property IsUnselectOnExit: Boolean read FIsUnselectOnExit write FIsUnselectOnExit;
    property IsShowZeros: Boolean read FIsShowZeros write SetShowZeros;

    property SelectedRowIndex: Integer read FSelectedRowIndex;
    property SelectedColIndex: Integer read FSelectedColIndex;
    property SelectedText: String read GetSelectedText write SetSelectedText;

    property ColorColumnBorderColor: TColor read FColorColumnBorderColor write FColorColumnBorderColor;
    property ColorColumnCellMargin: Integer read FColorColumnCellMargin write FColorColumnCellMargin;

    property OnEdititingDone: TVSTEdititingDoneEvent read FOnEdititingDone write FOnEdititingDone;
    property OnEdititingBegin: TVSTEvent read FOnEdititingBegin write FOnEdititingBegin;
  end;

implementation

{ TVSTEdit }

function TVSTEdit.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  Result:= (Node^.Index=FSelectedRowIndex) and (Column=FSelectedColIndex);
end;

function TVSTEdit.GetColumnRowTitlesBGColor: TColor;
begin
  Result:= clNone;
  if not IsColumnRowTitlesExists then Exit;
  Result:= FColumnValuesBGColors[FTitleColumnIndex];
end;

procedure TVSTEdit.SetColumnRowTitlesBGColor(AValue: TColor);
begin
  if not IsColumnRowTitlesExists then Exit;
  if FColumnValuesBGColors[FTitleColumnIndex]=AValue then Exit;
  FColumnValuesBGColors[FTitleColumnIndex]:= AValue;
  FTree.Refresh;
end;

procedure TVSTEdit.SetColumnRowTitlesFont(AValue: TFont);
begin
  FColumnRowTitlesFont.Assign(AValue);
  FTree.Refresh;
end;

procedure TVSTEdit.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  i, n: Integer;
begin
  if High(FDataValues)<Column then Exit;
  i:= Node^.Index;
  CellText:= EmptyStr;
  if VIsNil(FDataValues[Column]) then Exit;
  if FColumnTypes[Column]=ctKeyPick then
  begin
    if SEmpty(FDataValues[Column, i]) then Exit;
    n:= VIndexOf(FKeys[Column], StrToInt(FDataValues[Column, i]));
    if n>=0 then
      CellText:= FPicks[Column, n]
    else
      CellText:= FDataValues[Column, i];
  end
  else begin
    if (FDataValues[Column, i]='0') and (not IsShowZeros) then Exit;
    CellText:= FDataValues[Column, i];
  end;
end;

procedure TVSTEdit.Select(const ARowIndex, AColIndex: Integer);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(ARowIndex);
  if not Assigned(Node) then Exit;
  if not IsEditingColIndexCorrect(AColIndex) then Exit;
  SelectCell(Node, AColIndex);
end;

procedure TVSTEdit.Select(const ARowIndex: Integer; const AColumnCaption: String);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, AColumnCaption);
  if not IsEditingColIndexCorrect(ColIndex) then Exit;
  Select(ARowIndex, ColIndex);
end;

procedure TVSTEdit.Select(const ARowTitle, AColumnCaption: String);
var
  RowIndex: Integer;
begin
  if not IsColumnRowTitlesExists then Exit;
  RowIndex:= VIndexOf(FDataValues[FTitleColumnIndex], ARowTitle);
  if not IsRowIndexCorrect(RowIndex) then Exit;
  Select(RowIndex, AColumnCaption);
end;

procedure TVSTEdit.Show(const ARowIndex: Integer);
var
  Node: PVirtualNode;
begin
  if not IsRowIndexCorrect(ARowIndex) then Exit;
  Node:= NodeFromIndex(ARowIndex);
  if not Assigned(Node) then Exit;
  FTree.FocusedNode:= Node;
end;

function TVSTEdit.IsColumnValuesExists(const AColIndex: Integer): Boolean;
begin
  Result:= False;
  if not IsEditingColIndexCorrect(AColIndex) then Exit;
  if MIsNil(FDataValues) then Exit;
  if VIsNil(FDataValues[AColIndex]) then Exit;
  Result:= True;
end;

function TVSTEdit.ColumnAsInteger(out AValues: TIntVector;
  const AColIndex: Integer; const ADefaultValue: Integer): Boolean;
var
  i: Integer;
  Value: Integer;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
    if TryStrToInt(FDataValues[AColIndex,i], Value) then
      AValues[i]:= Value;
end;

function TVSTEdit.ColumnAsDouble(out AValues: TDblVector;
  const AColIndex: Integer; const ADefaultValue: Integer): Boolean;
var
  i: Integer;
  Value: Double;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
    if TryStrToFloat(FDataValues[AColIndex,i], Value) then
      AValues[i]:= Value;
end;

function TVSTEdit.ColumnAsString(out AValues: TStrVector;
  const AColIndex: Integer; const ADefaultValue: String): Boolean;
var
  i: Integer;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
      AValues[i]:= FDataValues[AColIndex,i];
end;

function TVSTEdit.ColumnAsDate(out AValues: TDblVector;
  const AColIndex: Integer; const ADefaultValue: TDate): Boolean;
var
  i: Integer;
  Value: TDate;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
    if TryStrToDate(FDataValues[AColIndex,i], Value) then
      AValues[i]:= Value;
end;

function TVSTEdit.ColumnAsTime(out AValues: TDblVector;
  const AColIndex: Integer; const ADefaultValue: TTime): Boolean;
var
  i: Integer;
  Value: TTime;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
    if TryStrToTime(FDataValues[AColIndex,i], Value) then
      AValues[i]:= Value;
end;

function TVSTEdit.ColumnAsColor(out AValues: TColorVector;
  const AColIndex: Integer; const ADefaultValue: TColor): Boolean;
var
  i: Integer;
  Value: Integer;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
    if TryStrToInt(FDataValues[AColIndex,i], Value) then
      AValues[i]:= TColor(Value);
end;

function TVSTEdit.ColumnAsInteger(out AValues: TIntVector;
  const ACaption: String; const ADefaultValue: Integer): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsInteger(AValues, ColIndex, ADefaultValue);
end;

function TVSTEdit.ColumnAsDouble(out AValues: TDblVector;
  const ACaption: String; const ADefaultValue: Integer): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsDouble(AValues, ColIndex, ADefaultValue);
end;

function TVSTEdit.ColumnAsString(out AValues: TStrVector;
  const ACaption: String; const ADefaultValue: String): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsString(AValues, ColIndex, ADefaultValue);
end;

function TVSTEdit.ColumnAsDate(out AValues: TDblVector; const ACaption: String;
  const ADefaultValue: TDate): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsDate(AValues, ColIndex, ADefaultValue);
end;

function TVSTEdit.ColumnAsTime(out AValues: TDblVector; const ACaption: String;
  const ADefaultValue: TTime): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsTime(AValues, ColIndex, ADefaultValue);
end;

function TVSTEdit.ColumnAsColor(out AValues: TColorVector;
  const ACaption: String; const ADefaultValue: TColor): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsColor(AValues, ColIndex, ADefaultValue);
end;

procedure TVSTEdit.RowInsert(const ARowIndex: Integer; const AValues: TStrVector = nil);
var
  Node: PVirtualNode;
  i: Integer;
begin
  MRowIns(FDataValues, ARowIndex, AValues);
  for i:= 0 to High(FDataValues) do
  begin
    if FColumnTypes[i]=ctKeyPick then
      FDataValues[i, ARowIndex]:= IntToStr(FKeys[i, 0]);
  end;
  Node:= NodeFromIndex(ARowIndex);
  if Assigned(Node) then
    FTree.InsertNode(Node, amInsertBefore)
  else
    FTree.AddChild(FTree.RootNode);
  FTree.Refresh;
end;

procedure TVSTEdit.RowDelete(const ARowIndex: Integer);
var
  Node: PVirtualNode;
begin
  if not IsRowIndexCorrect(ARowIndex) then Exit;
  Node:= NodeFromIndex(ARowIndex);
  if not Assigned(Node) then Exit;
  MRowDel(FDataValues, ARowIndex);
  FTree.DeleteNode(Node);
  FTree.Refresh;
end;

function TVSTEdit.GetRowValues(const ARowIndex: Integer): TStrVector;
begin
  Result:= nil;
  if not IsRowIndexCorrect(ARowIndex) then Exit;
  Result:= MRowGet(FDataValues, ARowIndex);
end;

procedure TVSTEdit.SetRowValues(const ARowIndex: Integer; const ARowValues: TStrVector);
begin
  MRowSet(FDataValues, ARowIndex, ARowValues);
  FTree.Refresh;
end;

procedure TVSTEdit.SetColumnRowTitlesHeaderBGColor(const ABGColor: TColor);
begin
  if not IsColumnRowTitlesExists then Exit;
  SetColumnHeaderBGColor(FTitleColumnIndex, ABGColor);
end;

procedure TVSTEdit.SetShowZeros(AValue: Boolean);
begin
  if FIsShowZeros=AValue then Exit;
  FIsShowZeros:= AValue;
  FTree.Refresh;
end;

function TVSTEdit.CellRectangle(Column: TColumnIndex; ACellRect: TRect): TRect;
begin
  Result:= inherited CellRectangle(Column, ACellRect);
  if FColumnTypes[Column]=ctColor then
    Result.Inflate(-FColorColumnCellMargin,-FColorColumnCellMargin,
                   -FColorColumnCellMargin,-FColorColumnCellMargin);
end;

function TVSTEdit.CellGridColor(Node: PVirtualNode; Column: TColumnIndex; ABGColor: TColor): TColor;
begin
  Result:= inherited CellGridColor(Node, Column, ABGColor);
  if (FColumnTypes[Column]=ctColor) and (ColorColumnBorderColor<>clNone) then
    Result:= FColorColumnBorderColor;
end;

procedure TVSTEdit.SelectCell(Node: PVirtualNode; Column: TColumnIndex; const ASaveChanges: Boolean = True);
var
  RowIndex, ColIndex: Integer;
begin
  if (not Assigned(Node)) and (Column=-1) then  //unselect
  begin
    if IsEditing and IsOneRowEditing then Exit;
    EndEdit(ASaveChanges);
    if FCanUnselect then
    begin
      FSelectedRowIndex:= -1;
      FSelectedColIndex:= -1;
    end;
  end
  else if Assigned(Node) and (Column<>FTitleColumnIndex) then //select
  begin
    if IsEditing and IsOneRowEditing and (Node^.Index<>FSelectedRowIndex) then Exit;
    EndEdit(ASaveChanges);
    RowIndex:= FSelectedRowIndex;
    ColIndex:= FSelectedColIndex;
    FSelectedRowIndex:= Node^.Index;
    FSelectedColIndex:= Column;
    FTree.FocusedNode:= Node;
    if (FSelectedRowIndex=RowIndex) and (FSelectedColIndex=ColIndex) then
      BeginEdit;
  end;

  if Assigned(FOnSelect) then FOnSelect;
  FTree.Refresh;
end;

procedure TVSTEdit.UnselectCell(const ASaveChanges: Boolean = True);
begin
  SelectCell(nil, -1, ASaveChanges);
end;

procedure TVSTEdit.NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  SelectCell(HitInfo.HitNode, HitInfo.HitColumn);
end;

procedure TVSTEdit.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then
  begin
    if FCanUnselect then
      UnselectCell;
  end
end;

procedure TVSTEdit.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  R: Integer;
begin
  if FColumnTypes[FSelectedColIndex]=ctColor then
  begin
    Key:= VK_UNKNOWN;
    Exit;
  end;

  if Key=VK_RETURN then
  begin
    R:= FSelectedRowIndex + 1;
    if not IsRowIndexCorrect(R) then
      EditorExit
    else begin
      PostMessage(FTree.Handle, LM_KEYDOWN, VK_DOWN, 0);
      PostMessage(FTree.Handle, LM_KEYDOWN, VK_RETURN, 0);
    end;
  end
  else if Key=VK_ESCAPE then
    EditorExit;
end;

procedure TVSTEdit.EditorExit;
begin
  PostMessage(FTree.Handle, LM_KEYDOWN, VK_ESCAPE, 0);
  FTree.SetFocus;
end;

procedure TVSTEdit.TreeExit(Sender: TObject);
begin
  EditingDone;
end;

procedure TVSTEdit.TreeColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  EditingDone;
end;

procedure TVSTEdit.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not IsSelected then Exit;
  case Key of
   VK_RETURN: if IsBeginEditOnKeyPress then BeginEdit;
   VK_DELETE, VK_BACK: DeleteSelectedCellText;
   VK_ESCAPE: if Assigned(FEditor) then
                MoveSelectionVertical(0)
              else
                UnSelect;
   VK_DOWN:   MoveSelectionVertical(1);
   VK_UP:     MoveSelectionVertical(-1);
   VK_RIGHT:  MoveSelectionHorizontal(1);
   VK_LEFT:   MoveSelectionHorizontal(-1);
  end;
end;

procedure TVSTEdit.UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  n: Integer;
  Key: Word;
begin
  if not IsSelected then Exit;
  if not IsBeginEditOnKeyPress then Exit;

  case FColumnTypes[FSelectedColIndex] of
  ctInteger:
    begin
      if SPos(SYMBOLS_DIGITS, UTF8Key)=0 then Exit;
      FDataValues[FSelectedColIndex, FSelectedRowIndex]:= UTF8Key;
      BeginEdit;
    end;
  ctString:
    begin
      if SPos(SYMBOLS_KEYBOARD, UTF8Key)=0 then Exit;
      FDataValues[FSelectedColIndex, FSelectedRowIndex]:= UTF8Key;
      BeginEdit;
    end;
  ctDate, ctTime:
    begin
      n:= SPos(SYMBOLS_DIGITS, UTF8Key);
      if n=0 then Exit;
      BeginEdit;
      Key:= VK_0;
      if n<10 then
        Key:= Key + n;
      PostMessage((FEditor as TWinControl).Handle, LM_KEYDOWN, Key, 0);
    end;
    ctDouble:
      begin
        if SPos(SYMBOLS_DIGITS+SYMBOL_COMMA+SYMBOL_DOT, UTF8Key)=0 then Exit;
        if SSame(UTF8Key, SYMBOL_COMMA) or SSame(UTF8Key, SYMBOL_DOT) then
          UTF8Key:= DefaultFormatSettings.DecimalSeparator;
        FDataValues[FSelectedColIndex, FSelectedRowIndex]:= UTF8Key;
        BeginEdit;
      end;
  //ctKeyPick: ; //not need
  //ctColor:   ; //not need
  end;

  FTree.Refresh;
end;

constructor TVSTEdit.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
  FTree.OnGetText:= @GetText;
  FTree.OnNodeClick:= @NodeClick;
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnKeyDown:= @KeyDown;
  FTree.OnUTF8KeyPress:= @UTF8KeyPress;
  FTree.OnExit:= @TreeExit;
  FTree.OnColumnResize:= @TreeColumnResize;

  FColumnRowTitlesFont:= TFont.Create;
  FColumnRowTitlesFont.Assign(FTree.Font);

  FTitleColumnIndex:= -1;
  FSelectedRowIndex:= -1;
  FSelectedColIndex:= -1;

  FIsShowZeros:= False;
  FIsUnselectOnExit:= True;
  FIsEditing:= False;
  FIsOneRowEditing:= False;
  FIsBeginEditOnKeyPress:= True;

  FCellTextBeforeEditing:= 'FCellTextBeforeEditing';
  FColorColumnCellMargin:= 0;
  FColorColumnBorderColor:= clNone;
end;

destructor TVSTEdit.Destroy;
begin
  FreeAndNil(FColumnRowTitlesFont);
  if Assigned(FEditor) then FreeAndNil(FEditor);
  inherited Destroy;
end;

procedure TVSTEdit.EditingDone;
begin
  if not IsEditing then Exit;
  if CanUnselect then
  begin
    if IsUnselectOnExit then
      Unselect
    else
      EndEdit(False);
  end;
end;

function TVSTEdit.GetIsRowTitlesColumnExists: Boolean;
begin
  Result:= FTitleColumnIndex>=0;
end;

function TVSTEdit.CellBGColor(Node: PVirtualNode; Column: TColumnIndex): TColor;
begin
  if FColumnTypes[Column]=ctColor then
    Result:= StrToInt(FDataValues[Column, Node^.Index])
  else
    Result:= inherited CellBGColor(Node, Column);
end;

procedure TVSTEdit.CellFont(Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column=FTitleColumnIndex then
    FCellFont.Assign(FColumnRowTitlesFont)
  else begin
    inherited CellFont(Node, Column);
    if FColumnTypes[Column]=ctColor then
      FCellFont.Color:= StrToInt(FDataValues[Column, Node^.Index]);
  end;
end;

procedure TVSTEdit.DeleteSelectedCellText;
begin
  FDataValues[FSelectedColIndex, FSelectedRowIndex]:= EmptyStr;
  FTree.Refresh;
end;

procedure TVSTEdit.MoveSelectionVertical(const ADirection: Integer);
var
  NewRowIndex: Integer;
begin
  if not IsSelected then Exit;
  NewRowIndex:= FSelectedRowIndex + ADirection;
  if not IsRowIndexCorrect(NewRowIndex) then Exit;
  Select(NewRowIndex, FSelectedColIndex);
end;

procedure TVSTEdit.MoveSelectionHorizontal(const ADirection: Integer);
var
  NewColIndex: Integer;
begin
  if not IsSelected then Exit;
  NewColIndex:= FSelectedColIndex;
  repeat
    NewColIndex:= NewColIndex + ADirection;
  until NewColIndex<>FTitleColumnIndex;
  if not IsColIndexCorrect(NewColIndex) then Exit;
  Select(FSelectedRowIndex, NewColIndex);
end;

procedure TVSTEdit.BeginEdit;
var
  ColumnType: TVSTColumnType;

  procedure CreateEditorInteger;
  begin
    FEditor:= TSpinEdit.Create(FTree);
    TSpinEdit(FEditor).Text:= SelectedText;
    TSpinEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure CreateEditorDouble;
  begin
    FEditor:= TFloatSpinEdit.Create(FTree);
    TFloatSpinEdit(FEditor).Text:= SelectedText;
    TFloatSpinEdit(FEditor).DecimalPlaces:= FDecimalPlaces[FSelectedColIndex];
    TFloatSpinEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure CreateEditorString;
  begin
    FEditor:= TEdit.Create(FTree);
    TEdit(FEditor).Text:= SelectedText;
    TEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure CreateEditorDate;
  begin
    FEditor:= TDateTimePicker.Create(FTree);
    TDateTimePicker(FEditor).Kind:= dtkDate;
    TDateTimePicker(FEditor).Alignment:=FTree.Header.Columns[FSelectedColIndex].Alignment;
    TDateTimePicker(FEditor).Date:= StrToDateDef(SelectedText, Date);
  end;

  procedure CreateEditorTime;
  begin
    FEditor:= TDateTimePicker.Create(FTree);
    TDateTimePicker(FEditor).Kind:= dtkTime;
    TDateTimePicker(FEditor).TimeDisplay:= tdHMSMs;
    TDateTimePicker(FEditor).TimeFormat:= tf24;
    TDateTimePicker(FEditor).Alignment:=FTree.Header.Columns[FSelectedColIndex].Alignment;
    TDateTimePicker(FEditor).Time:= StrToTimeDef(SelectedText, 0);
  end;

  procedure CreateEditorKeyPick;
  var
    n: Integer;
  begin
    FEditor:= TBCButton.Create(FTree);
    FEditor.Parent:= FTree;
    FDropDown:= TDropDown.Create(FEditor as TBCButton);
    FDropDown.Font:= ValuesFont;
    FDropDown.Items:= FPicks[FSelectedColIndex];
    n:= VIndexOf(FKeys[FSelectedColIndex], StrToInt(SelectedText));
    if n>=0 then
      FDropDown.ItemIndex:= n;
  end;

  procedure CreateEditorColor;
  begin
    FEditor:= TEdit.Create(FTree);
    TEdit(FEditor).Color:= StrToInt(SelectedText);
    TEdit(FEditor).Font.Color:= TEdit(FEditor).Color;
    TEdit(FEditor).Text:= SelectedText;
    TEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure GoEditInteger;
  begin
    TSpinEdit(FEditor).SetFocus;
    TSpinEdit(FEditor).SelStart:= SLength(TSpinEdit(FEditor).Text);
  end;

  procedure GoEditDouble;
  begin
    TFloatSpinEdit(FEditor).SetFocus;
    TFloatSpinEdit(FEditor).SelStart:= SLength(TFloatSpinEdit(FEditor).Text);
  end;

  procedure GoEditString;
  begin
    TEdit(FEditor).SetFocus;
    TEdit(FEditor).SelStart:= SLength(TEdit(FEditor).Text);
  end;

  procedure GoEditDate;
  begin
    TDateTimePicker(FEditor).SetFocus;
  end;

  procedure GoEditTime;
  begin
    TDateTimePicker(FEditor).SetFocus;
  end;

  procedure GoEditKeyPick;
  begin
    FDropDown.Expand;
  end;

  procedure GoEditColor;
  var
    ColorDialog: TColorDialog;
  begin
    ColorDialog:= TColorDialog.Create(nil);
    try
      if ColorDialog.Execute then
      begin
        TEdit(FEditor).Text:= IntToStr(ColorToRGB(ColorDialog.Color));
        TEdit(FEditor).Color:= ColorDialog.Color;
        TEdit(FEditor).Font.Color:= ColorDialog.Color;
      end;
      Unselect;
    finally
      FreeAndNil(ColorDialog);
    end;
  end;

begin
  FIsEditing:= True;
  ColumnType:= FColumnTypes[FSelectedColIndex];

  if Assigned(FOnEdititingBegin) then
    FOnEdititingBegin;

  if FCellTextBeforeEditing='FCellTextBeforeEditing' then
  begin
    FCellTextBeforeEditing:= SelectedText;
    if Assigned(FEditor) then FreeAndNil(FEditor);
    case ColumnType of
      ctInteger: CreateEditorInteger;
      ctString:  CreateEditorString;
      ctDate:    CreateEditorDate;
      ctTime:    CreateEditorTime;
      ctKeyPick: CreateEditorKeyPick;
      ctColor:   CreateEditorColor;
      ctDouble:  CreateEditorDouble;
    else
      Exit;
    end;
    if FEditor is TWinControl then
      (FEditor as TWinControl).OnKeyDown:= @EditorKeyDown;
    FEditor.Parent:= FTree;
    FEditor.AutoSize:= False;
    FEditor.BoundsRect:= SelectedCellRect;
    FEditor.Show;
  end;

  case ColumnType of
    ctInteger: GoEditInteger;
    ctString:  GoEditString;
    ctDate:    GoEditDate;
    ctTime:    GoEditTime;
    ctKeyPick: GoEditKeyPick;
    ctColor:   GoEditColor;
    ctDouble:  GoEditDouble;
  end;
end;

procedure TVSTEdit.EndEdit(const ASaveChanges: Boolean);
var
  ColumnType: TVSTColumnType;
begin
  if not Assigned(FEditor) then Exit;

  ColumnType:= FColumnTypes[FSelectedColIndex];
  if ASaveChanges then
  begin
    case ColumnType of
      ctInteger: if (TSpinEdit(FEditor).Value=0) and (not IsShowZeros) then
                   SelectedText:= EmptyStr
                 else
                   SelectedText:= IntToStr(TSpinEdit(FEditor).Value);
      ctString:  SelectedText:= TEdit(FEditor).Text;
      ctDate:    SelectedText:= FormatDateTime(
                                      FColumnFormatStrings[FSelectedColIndex],
                                      TDateTimePicker(FEditor).Date);
      ctTime:    SelectedText:= FormatDateTime(
                                      FColumnFormatStrings[FSelectedColIndex],
                                      TDateTimePicker(FEditor).Time);
      ctKeyPick: SelectedText:= IntToStr(FKeys[FSelectedColIndex, FDropDown.ItemIndex]);
      ctColor:   SelectedText:= TEdit(FEditor).Text;
      ctDouble:  if (TFloatSpinEdit(FEditor).Value=0) and (not IsShowZeros) then
                   SelectedText:= EmptyStr
                 else
                   SelectedText:= FloatToStr(TFloatSpinEdit(FEditor).Value);
    end;
  end
  else begin
    SelectedText:= FCellTextBeforeEditing;
  end;
  FCellTextBeforeEditing:= 'FCellTextBeforeEditing';

  if Assigned(FOnEdititingDone) then
    FOnEdititingDone(FSelectedRowIndex, FSelectedColIndex, SelectedText, ColumnType, ASaveChanges);

  FIsEditing:= False;
  if ColumnType=ctKeyPick then
  begin
    FDropDown.Collapse;
    FreeAndNil(FDropDown);
  end;
  FreeAndNil(FEditor);
end;

procedure TVSTEdit.SetColumnRowTitlesVisible(AValue: Boolean);
begin
  if not IsColumnRowTitlesExists then Exit;
  if AValue then
    FTree.Header.Columns[FTitleColumnIndex].Options:=
      FTree.Header.Columns[FTitleColumnIndex].Options + [coVisible]
  else
    FTree.Header.Columns[FTitleColumnIndex].Options:=
      FTree.Header.Columns[FTitleColumnIndex].Options - [coVisible];
  FTree.Refresh;
end;

function TVSTEdit.GetIsSelected: Boolean;
begin
  Result:= False;
  if not Assigned(Self) then Exit;
  Result:= (FSelectedRowIndex>=0) and (FSelectedColIndex>=0);
end;

function TVSTEdit.GetSelectedText: String;
begin
  Result:= EmptyStr;
  if not IsSelected then Exit;
  Result:= FDataValues[FSelectedColIndex, FSelectedRowIndex];
end;

procedure TVSTEdit.SetSelectedText(AValue: String);
begin
  if not IsSelected then Exit;
  if FDataValues[FSelectedColIndex, FSelectedRowIndex]=AValue then Exit;
  FDataValues[FSelectedColIndex, FSelectedRowIndex]:= AValue;
  FTree.Refresh;
end;

procedure TVSTEdit.HeaderClear;
begin
  inherited HeaderClear;
  FDataValues:= nil;
  FColumnTypes:= nil;
  FColumnFormatStrings:= nil;
  FDecimalPlaces:= nil;
  FTitleColumnIndex:= -1;
end;

procedure TVSTEdit.ValuesClear;
begin
  FSelectedRowIndex:= -1;
  FSelectedColIndex:= -1;
  inherited ValuesClear;
end;

procedure TVSTEdit.Draw;
var
  i, n: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;

  if IsColumnRowTitlesExists then
  begin
    n:= Length(FDataValues[FTitleColumnIndex]);
    for i:= 0 to High(FDataValues) do
      if i<>FTitleColumnIndex then
        VReDim(FDataValues[i], n, EmptyStr);
    VSTLoad(FTree, FDataValues[FTitleColumnIndex]);
  end;

  inherited Draw;
end;

procedure TVSTEdit.AutosizeColumnRowTitlesEnable;
begin
  if not IsColumnRowTitlesExists then Exit;
  AutosizeColumnEnable(FTitleColumnIndex);
end;

function TVSTEdit.SelectedCellRect: TRect;
var
  Node: PVirtualNode;
begin
  Result:= Rect(0,0,0,0);
  if not IsSelected then Exit;
  Node:= NodeFromIndex(FSelectedRowIndex);
  Result:= FTree.GetDisplayRect(Node, FSelectedColIndex, False);
  if IsColumnRowTitlesExists then
    if FSelectedColIndex=FTitleColumnIndex+1 then
      Result.Left:= Result.Left + 1;
  Result.Bottom:= Result.Bottom - 1;
  if Result.Top<=0 then Result.Top:= 1;
end;

function TVSTEdit.IsEditingColIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= (AIndex<>FTitleColumnIndex) and IsColIndexCorrect(AIndex);
end;

function TVSTEdit.IsRowIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if not IsColumnRowTitlesExists then Exit;
  if MIsNil(FDataValues) then Exit;
  Result:= (AIndex>=0) and (AIndex<=High(FDataValues[FTitleColumnIndex]));
end;

procedure TVSTEdit.AddColumnRowTitles(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
begin
  if IsColumnRowTitlesExists then Exit;
  AddColumn(ACaption, AWidth, ACaptionAlignment);
  FTitleColumnIndex:= High(FHeaderCaptions);
  SetNewColumnSettings(ctUndefined, EmptyStr);
  MAppend(FKeys, nil);
  MAppend(FPicks, nil);
end;

procedure TVSTEdit.SetColumnRowTitles(const AValues: TStrVector;
  const AValuesAlignment: TAlignment);
begin
  if not IsColumnRowTitlesExists then Exit;
  FDataValues[FTitleColumnIndex]:= VCut(AValues);
  FTree.Header.Columns[FTitleColumnIndex].Alignment:= AValuesAlignment;
  FTree.Refresh;
end;

procedure TVSTEdit.SetNewColumnSettings(const AColumnType: TVSTColumnType;
  const AFormatString: String; const ADecimalPlaces: Integer = 0);
begin
  MAppend(FDataValues, nil);
  VAppend(FColumnFormatStrings, AFormatString);
  VAppend(FDecimalPlaces, ADecimalPlaces);
  SetLength(FColumnTypes, Length(FHeaderCaptions));
  FColumnTypes[High(FColumnTypes)]:= AColumnType;
end;

procedure TVSTEdit.AddColumnInteger(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment;
  const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctInteger, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddColumnDouble(const ACaption: String;
                        const ADecimalPlaces: Integer = 2;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
begin
  AddValuesColumn(ctDouble, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment,
                  nil, nil, ADecimalPlaces);
end;

procedure TVSTEdit.AddColumnString(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment;
  const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctString, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddColumnDate(const ACaption, AFormatString: String;
  const AWidth: Integer;
  const ACaptionAlignment: TAlignment; const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctDate, ACaption, AFormatString, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddColumnTime(const ACaption, AFormatString: String;
  const AWidth: Integer;
  const ACaptionAlignment: TAlignment; const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctTime, ACaption, AFormatString, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddColumnKeyPick(const ACaption: String;
                        const AKeys: TIntVector; const APicks: TStrVector;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
begin
  AddValuesColumn(ctKeyPick, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment, AKeys, APicks);
end;

procedure TVSTEdit.AddColumnColor(const ACaption: String; const AWidth: Integer;
                                  const ACaptionAlignment: TAlignment = taCenter);
begin
  AddValuesColumn(ctColor, ACaption, EmptyStr, AWidth, ACaptionAlignment);
end;

procedure TVSTEdit.SetColumnInteger(const ACaption: String; const AValues: TIntVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnInteger(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnDouble(const ACaption: String; const AValues: TDblVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnDouble(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnString(const ACaption: String; const AValues: TStrVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnString(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnDate(const ACaption: String; const AValues: TDateVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnDate(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnTime(const ACaption: String; const AValues: TTimeVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnTime(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnColor(const ACaption: String; const AValues: TColorVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnColor(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnInteger(const AColIndex: Integer; const AValues: TIntVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VIntToStr(AValues);
end;

procedure TVSTEdit.SetColumnDouble(const AColIndex: Integer; const AValues: TDblVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VFloatToStr(AValues);
end;

procedure TVSTEdit.SetColumnString(const AColIndex: Integer; const AValues: TStrVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VCut(AValues);
end;

procedure TVSTEdit.SetColumnDate(const AColIndex: Integer; const AValues: TDateVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VFormatDateTime(FColumnFormatStrings[AColIndex], AValues);
end;

procedure TVSTEdit.SetColumnTime(const AColIndex: Integer; const AValues: TTimeVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VFormatDateTime(FColumnFormatStrings[AColIndex], AValues);
end;

procedure TVSTEdit.SetColumnColor(const AColIndex: Integer; const AValues: TColorVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VIntToStr(AValues);
end;

procedure TVSTEdit.AddValuesColumn(const AColumnType: TVSTColumnType;
                             const ACaption, AFormatString: String;
                             const AWidth: Integer;
                             const ACaptionAlignment: TAlignment;
                             const AValuesAlignment: TAlignment;
                             const AKeys: TIntVector = nil;
                             const APicks: TStrVector = nil;
                             const ADecimalPlaces: Integer = 0);
begin
  AddColumn(ACaption, AWidth, ACaptionAlignment);
  FTree.Header.Columns[High(FHeaderCaptions)].Alignment:= AValuesAlignment;
  SetNewColumnSettings(AColumnType, AFormatString, ADecimalPlaces);
  MAppend(FKeys, AKeys);
  MAppend(FPicks, APicks);
end;

procedure TVSTEdit.UnSelect(const ASaveChanges: Boolean);
begin
  if IsSelected then
    UnselectCell(ASaveChanges);
end;

end.

