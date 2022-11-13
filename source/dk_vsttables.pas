unit DK_VSTTables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, VirtualTrees, StdCtrls, Spin,
  DateTimePicker, LMessages, LCLIntf,
  DK_VSTUtils, DK_Vector, DK_Matrix, DK_StrUtils, DK_Const;

type

  { TVSTCoreTable }

  TVSTCoreTable = class(TObject)
  protected
    FTree: TVirtualStringTree;

    FAutosizeColumnIndex: Integer; //-2 last clolumn, -1 none

    FGridLinesColor: TColor;
    FGridLinesVisible: Boolean;

    FCanSelect: Boolean;
    FCanRightMouseButtonUnselect: Boolean;

    FValuesBGColor: TColor;
    FHeaderBGColor: TColor;
    FSelectedBGColor: TColor;

    FHeaderCaptions: TStrVector;
    FColumnWidths: TIntVector;

    FHeaderFont: TFont;
    FValuesFont: TFont;
    FSelectedFont: TFont;

    function NodeFromIndex(const AIndex: Integer): PVirtualNode;
    function NodeFromIndex(const AIndex1, AIndex2: Integer): PVirtualNode;

    procedure SetCanSelect(AValue: Boolean); virtual;
    procedure HeaderClear; virtual;
    procedure SetHeaderVisible(AValue: Boolean);
    procedure SetColumnWidths;
    procedure SetGridLinesVisible(AValue: Boolean);
    procedure SetGridLinesColor(AValue: TColor);
    procedure SetHeaderBGColor(AValue: TColor);
    procedure SetHeaderFont(AValue: TFont);
    procedure SetSelectedBGColor(AValue: TColor);
    procedure SetSelectedFont(AValue: TFont);
    procedure SetValuesBGColor(AValue: TColor);
    procedure SetValuesFont(AValue: TFont);
    procedure HeaderDrawQueryElements(Sender: TVTHeader;
                            var {%H-}PaintInfo: THeaderPaintInfo;
                            var Elements: THeaderPaintElements);
    procedure AdvancedHeaderDraw(Sender: TVTHeader;
                            var PaintInfo: THeaderPaintInfo;
                            const Elements: THeaderPaintElements);
    procedure BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                              Node: PVirtualNode; Column: TColumnIndex;
                              {%H-}CellPaintMode: TVTCellPaintMode; CellRect: TRect;
                              var {%H-}ContentRect: TRect);
    procedure DrawText(Sender: TBaseVirtualTree;
                       TargetCanvas: TCanvas; Node: PVirtualNode;
                       {%H-}Column: TColumnIndex;
                       const {%H-}CellText: String; const {%H-}CellRect: TRect;
                       var {%H-}DefaultDraw: Boolean);

    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual;

    function CellBGColor(Node: PVirtualNode; {%H-}Column: TColumnIndex): TColor; virtual;
    function CellFont(Node: PVirtualNode; {%H-}Column: TColumnIndex): TFont; virtual;
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure ValuesClear; virtual;
    procedure Clear;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter); virtual;

    procedure AutosizeColumnEnable(const AColumnIndex: Integer);
    procedure AutosizeColumnDisable;

    property GridLinesColor: TColor read FGridLinesColor write SetGridLinesColor;
    property ValuesBGColor: TColor read FValuesBGColor write SetValuesBGColor;
    property HeaderBGColor: TColor read FHeaderBGColor write SetHeaderBGColor;
    property SelectedBGColor: TColor read FSelectedBGColor write SetSelectedBGColor;

    property CanSelect: Boolean read FCanSelect write SetCanSelect;
    property CanRightMouseButtonUnselect: Boolean read FCanRightMouseButtonUnselect write FCanRightMouseButtonUnselect;

    property GridLinesVisible: Boolean write SetGridLinesVisible;
    property HeaderVisible: Boolean write SetHeaderVisible;

    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property ValuesFont: TFont read FValuesFont write SetValuesFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;

  end;

  TVSTColumnType = (
    ctUndefined,
    ctInteger,
    ctString,
    ctDate,
    ctTime
  );

  { TVSTEdit }

  TVSTEdit = class (TVSTCoreTable)
  protected
    FDataValues: TStrMatrix;
    FColumnTypes: array of TVSTColumnType;
    FColumnFormatStrings: TStrVector;
    FSelectedRowIndex, FSelectedColIndex: Integer;
    FTitleColumnIndex: Integer;
    FEditor: TWinControl;

    FRowTitlesFont: TFont;
    FRowTitlesBGColor: TColor;


    procedure SelectCell(Node: PVirtualNode; Column: TColumnIndex);
    procedure UnselectCell;

    procedure HeaderClear; override;

    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    function GetIsSelected: Boolean;
    function GetSelectedText: String;
    procedure SetSelectedText(AValue: String);
    function SelectedCellRect: TRect;


    procedure SetRowTitlesBGColor(AValue: TColor);
    procedure SetRowTitlesFont(AValue: TFont);
    procedure SetRowTitlesColumnVisible(AValue: Boolean);
    procedure SetNewColumn(const AColumnType: TVSTColumnType;
                           const AFormatString: String);
    procedure AddValuesColumn(const AColumnType: TVSTColumnType;
                        const ACaption, AFormatString: String;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);

    function GetIsRowTitlesColumnExists: Boolean;

    function CellBGColor(Node: PVirtualNode; Column: TColumnIndex): TColor; override;
    function CellFont(Node: PVirtualNode; Column: TColumnIndex): TFont; override;

    procedure MoveSelectionVertical(const ADirection {1 down, -1 up}: Integer);
    procedure MoveSelectionHorizontal(const ADirection {1 right, -1 left}: Integer);

    procedure BeginEdit;
    procedure EndEdit;

    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure KeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure EditorKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure ValuesClear; override;
    procedure Draw; //virtual;



    procedure AddRowTitlesColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AFixColumn: Boolean = False);
    procedure SetRowTitlesColumn(const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddIntegerColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddStringColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddDateColumn(const ACaption, AFormatString: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddTimeColumn(const ACaption, AFormatString: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);


    procedure UnSelect;
    procedure Select(const ARowIndex, AColIndex: Integer);
    procedure Select(const ARowIndex: Integer; const AColumnCaption: String);
    procedure Select(const ARowTitle, AColumnCaption: String);


    property RowTitlesFont: TFont read FRowTitlesFont write SetRowTitlesFont;
    property RowTitlesBGColor: TColor read FRowTitlesBGColor write SetRowTitlesBGColor;

    property IsRowTitlesColumnExists: Boolean read GetIsRowTitlesColumnExists;
    property RowTitlesColumnVisible: Boolean write SetRowTitlesColumnVisible;

    property IsSelected: Boolean read GetIsSelected;
    property SelectedRowIndex: Integer read FSelectedRowIndex;
    property SelectedColIndex: Integer read FSelectedColIndex;
    property SelectedText: String read GetSelectedText write SetSelectedText;
  end;

  { TVSTCustomTable }

  TVSTCustomTable = class(TVSTCoreTable)
  protected
    FDataValues: TStrMatrix;
    FSelected: TBoolVector;

    function GetIsSelected: Boolean;
    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;

    procedure HeaderClear; override;

    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);

    function IsIndexCorrect(const AIndex: Integer): Boolean;
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure ValuesClear; override;
    procedure Draw; virtual;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter); override;
    procedure SetColumn(const AColumnIndex: Integer; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetColumn(const ACaption: String; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);

    procedure Show(const AIndex: Integer);

    property IsSelected: Boolean read GetIsSelected;

  end;


  { TVSTTable }

  TVSTTable = class(TVSTCustomTable)
  protected
    procedure SelectNode(Node: PVirtualNode);
    procedure UnselectNode;

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure KeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);

    procedure SetCanSelect(AValue: Boolean); override;
    function GetSelectedIndex: Integer;

    procedure MoveSelection(const ADeltaIndex: Integer);

  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure ValuesClear; override;

    procedure Draw; override;

    procedure UnSelect;
    procedure Select(const AIndex: Integer);
    procedure Select(const AColumnIndex: Integer; const AValue: String);
    procedure Select(const AColumnCaption, AValue: String);
    property SelectedIndex: Integer read GetSelectedIndex;

  end;

  { TVSTCheckTable }

  TVSTCheckTable = class(TVSTCustomTable)
  protected
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var {%H-}Allowed: Boolean);

    function GetIsAllChecked: Boolean;
    function GetIsAllUnchecked: Boolean;

    procedure SetCanSelect(AValue: Boolean); override;

    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    function GetChecked(AIndex: Integer): Boolean;
    function GetSelected: TBoolVector;

    procedure CheckNode(Node: PVirtualNode; const AChecked: Boolean);
    procedure Check(Node: PVirtualNode);
    procedure Check(const AIndex: Integer);
    procedure Uncheck(Node: PVirtualNode);
    procedure Uncheck(const AIndex: Integer);
    procedure ReverseCheckState(Node: PVirtualNode);
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;
    procedure ValuesClear; override;
    procedure Draw; override;

    procedure CheckAll(const AChecked: Boolean);

    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
    property IsAllChecked: Boolean read GetIsAllChecked;
    property IsAllUnchecked: Boolean read GetIsAllUnchecked;
    property Selected: TBoolVector read GetSelected;
  end;

  { TVSTCategoryCustomTable }

  TVSTCategoryCustomTable = class(TVSTCoreTable)
  protected
    FCategoryValues: TStrVector;
    FDataValues: TStrMatrix3D;
    FSelected: TBoolMatrix;

    procedure SetTreeLinesVisible(AValue: Boolean);
    procedure HeaderClear; override;
    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);
    function IsIndexesCorrect(const AIndex1, AIndex2: Integer): Boolean;

    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    function GetIsSelected: Boolean;
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter); override;
    procedure SetColumn(const AColumnIndex: Integer; const AValues: TStrMatrix;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetColumn(const ACaption: String; const AValues: TStrMatrix;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetCategories(const AValues: TStrVector);

    procedure Clear;
    procedure ValuesClear; override;

    procedure Draw;
    procedure ExpandAll(const AExpand: Boolean);
    procedure Show(const AIndex1, AIndex2: Integer);

    property TreeLinesVisible: Boolean write SetTreeLinesVisible;
    property IsSelected: Boolean read GetIsSelected;
  end;

  { TVSTCategoryRadioButtonTable }

  TVSTCategoryRadioButtonTable = class(TVSTCategoryCustomTable)
  protected

    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);

    procedure SetCanSelect(AValue: Boolean); override;
    procedure SelectNode(Node: PVirtualNode);
    procedure UnselectNode;
    function GetSelectedIndex1: Integer;
    function GetSelectedIndex2: Integer;
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure Draw;

    procedure ValuesClear; override;

    procedure Select(const AIndex1, AIndex2: Integer);
    procedure SelectedIndexes(out AIndex1, AIndex2: Integer);
    property SelectedIndex1: Integer read GetSelectedIndex1;
    property SelectedIndex2: Integer read GetSelectedIndex2;
  end;

  { TVSTCategoryCheckTable }

  TVSTCategoryCheckTable = class(TVSTCategoryCustomTable)
  protected

    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var {%H-}Allowed: Boolean);

    procedure SetCanSelect(AValue: Boolean); override;

    procedure SetChecked(AIndex1, AIndex2: Integer; AValue: Boolean);
    function GetChecked(AIndex1, AIndex2: Integer): Boolean;

    procedure CheckNode(Node: PVirtualNode; const AChecked: Boolean);
    procedure Check(Node: PVirtualNode);
    procedure Check(const AIndex1, AIndex2: Integer);
    procedure Uncheck(Node: PVirtualNode);
    procedure Uncheck(const AIndex1, AIndex2: Integer);
    procedure ReverseCheckState(Node: PVirtualNode);
    procedure ReverseCategoryCheckState(Node: PVirtualNode);

    function IsAllCheckedCategory(Node: PVirtualNode): Boolean;
    function IsAllUncheckedCategory(Node: PVirtualNode): Boolean;
    function IsHasCheckedCategory(Node: PVirtualNode): Boolean;
    function IsHasCheckedCategory(const AIndex: Integer): Boolean;
    procedure CheckCategory(Node: PVirtualNode; const AChecked: Boolean);
    procedure SetCategoryCheckState(Node: PVirtualNode);
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    function IsAllCheckedCategory(const AIndex: Integer): Boolean;
    function IsAllUncheckedCategory(const AIndex: Integer): Boolean;

    procedure CheckAll(const AChecked: Boolean);
    procedure CheckCategory(const AIndex: Integer; const AChecked: Boolean);

    property Checked[AIndex1, AIndex2: Integer]: Boolean read GetChecked write SetChecked;
  end;

implementation

{ TVSTEdit }



function TVSTEdit.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  Result:= (Node^.Index=FSelectedRowIndex) and (Column=FSelectedColIndex);
end;

procedure TVSTEdit.SetRowTitlesBGColor(AValue: TColor);
begin
  if FRowTitlesBGColor=AValue then Exit;
  FRowTitlesBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTEdit.SetRowTitlesFont(AValue: TFont);
begin
  FRowTitlesFont.Assign(AValue);
  FTree.Refresh;
end;

procedure TVSTEdit.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  i: Integer;
begin
  if High(FDataValues)<Column then Exit;
  i:= Node^.Index;
  CellText:= EmptyStr;
  if not VIsNil(FDataValues[Column]) then
    CellText:= FDataValues[Column, i];
end;

procedure TVSTEdit.Select(const ARowIndex, AColIndex: Integer);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(ARowIndex);
  if not Assigned(Node) then Exit;
  if not ((AColIndex<>FTitleColumnIndex) and
          (AColIndex>=0) and (AColIndex<=High(FHeaderCaptions))) then Exit;
  SelectCell(Node, AColIndex);
end;

procedure TVSTEdit.Select(const ARowIndex: Integer; const AColumnCaption: String);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, AColumnCaption);
  if (ColIndex<0) or (ColIndex=FTitleColumnIndex) then Exit;
  Select(ARowIndex, ColIndex);
end;

procedure TVSTEdit.Select(const ARowTitle, AColumnCaption: String);
var
  RowIndex: Integer;
begin
  if not IsRowTitlesColumnExists then Exit;
  RowIndex:= VIndexOf(FDataValues[FTitleColumnIndex], ARowTitle);
  if RowIndex<0 then Exit;
  Select(RowIndex, AColumnCaption);
end;



procedure TVSTEdit.SelectCell(Node: PVirtualNode; Column: TColumnIndex);
var
  RowIndex, ColIndex: Integer;
  IsEditorExists: Boolean;
begin
  if Column<>FTitleColumnIndex then
  begin
    IsEditorExists:= Assigned(FEditor);
    RowIndex:= FSelectedRowIndex;
    ColIndex:= FSelectedColIndex;
    UnselectCell;
    FSelectedRowIndex:= Node^.Index;
    FSelectedColIndex:= Column;
    FTree.FocusedNode:= Node;
    if (not IsEditorExists) and
       (FSelectedRowIndex=RowIndex) and (FSelectedColIndex=ColIndex) then
      BeginEdit;
  end;
  FTree.Refresh;
end;

procedure TVSTEdit.UnselectCell;
begin
  if not IsSelected then Exit;
  EndEdit;
  FSelectedRowIndex:= -1;
  FSelectedColIndex:= -1;
  FTree.Refresh;
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
    if FCanRightMouseButtonUnselect then
      UnselectCell;
  end
end;

procedure TVSTEdit.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  R: Integer;

  procedure EditorEscape;
  begin
    PostMessage(FTree.Handle, LM_KEYDOWN, VK_ESCAPE, 0);
    FTree.SetFocus;
  end;

begin
  if Key=VK_RETURN then
  begin
    R:= FSelectedRowIndex + 1;
    if (R<0) or (R>High(FDataValues[FTitleColumnIndex])) then
      EditorEscape
    else begin
      PostMessage(FTree.Handle, LM_KEYDOWN, VK_DOWN, 0);
      PostMessage(FTree.Handle, LM_KEYDOWN, VK_RETURN, 0);
    end;
  end
  else if Key=VK_ESCAPE then
    EditorEscape;
end;

procedure TVSTEdit.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not IsSelected then Exit;
  case Key of
   VK_RETURN: BeginEdit;
   VK_ESCAPE: if Assigned(FEditor) then
                MoveSelectionVertical(0)
              else
                UnSelect;
   VK_DOWN: MoveSelectionVertical(1);
   VK_UP:   MoveSelectionVertical(-1);
   VK_RIGHT: MoveSelectionHorizontal(1);
   VK_LEFT: MoveSelectionHorizontal(-1);
  end;

end;

procedure TVSTEdit.UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if not IsSelected then Exit;
  if SPos(SYMBOLS_KEYBOARD, UTF8Key)=0 then Exit;
  FDataValues[FSelectedColIndex, FSelectedRowIndex]:=
      FDataValues[FSelectedColIndex, FSelectedRowIndex] + UTF8Key;
  BeginEdit;
  FTree.Refresh;
end;



constructor TVSTEdit.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.Indent:= 0;
  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
  FTree.OnGetText:= @GetText;
  FTree.OnNodeClick:= @NodeClick;
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnKeyDown:= @KeyDown;
  FTree.OnUTF8KeyPress:= @UTF8KeyPress;

  FRowTitlesFont:= TFont.Create;
  FRowTitlesFont.Assign(FTree.Font);
  FRowTitlesBGColor:= clWindow;

  FTitleColumnIndex:= -1;
  FSelectedRowIndex:= -1;
  FSelectedColIndex:= -1;


end;

destructor TVSTEdit.Destroy;
begin
  FreeAndNil(FRowTitlesFont);
  if Assigned(FEditor) then FreeAndNil(FEditor);
  inherited Destroy;
end;

function TVSTEdit.GetIsRowTitlesColumnExists: Boolean;
begin
  Result:= FTitleColumnIndex>=0;
end;

function TVSTEdit.CellBGColor(Node: PVirtualNode; Column: TColumnIndex): TColor;
begin
  if Column=FTitleColumnIndex then
    Result:= FRowTitlesBGColor
  else
    Result:= inherited CellBGColor(Node, Column);
end;

function TVSTEdit.CellFont(Node: PVirtualNode; Column: TColumnIndex): TFont;
begin
  if Column=FTitleColumnIndex then
    Result:= FRowTitlesFont
  else
    Result:= inherited CellFont(Node, Column);
end;

procedure TVSTEdit.MoveSelectionVertical(const ADirection: Integer);
var
  NewRowIndex: Integer;
begin
  if not IsSelected then Exit;
  NewRowIndex:= FSelectedRowIndex + ADirection;
  if (NewRowIndex<0) or (NewRowIndex>High(FDataValues[FTitleColumnIndex])) then
    Exit;
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
  if (NewColIndex<0) or (NewColIndex>High(FHeaderCaptions)) then
    Exit;
  Select(FSelectedRowIndex, NewColIndex);
end;

procedure TVSTEdit.BeginEdit;
var
  ColumnType: TVSTColumnType;

  procedure CreateEditorInteger;
  begin
    FEditor:= TSpinEdit.Create(FTree);
    TSpinEdit(FEditor).BorderStyle:= bsNone;
    TSpinEdit(FEditor).Text:= SelectedText;
    TSpinEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure CreateEditorString;
  begin
    FEditor:= TEdit.Create(FTree);
    TEdit(FEditor).BorderStyle:= bsNone;
    TEdit(FEditor).Text:= SelectedText;
    TEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure CreateEditorDate;
  begin
    FEditor:= TDateTimePicker.Create(FTree);
    TDateTimePicker(FEditor).Kind:= dtkDate;
    TDateTimePicker(FEditor).BorderStyle:= bsNone;
    TDateTimePicker(FEditor).Alignment:=FTree.Header.Columns[FSelectedColIndex].Alignment;
    TDateTimePicker(FEditor).Date:= StrToDateDef(SelectedText, Date);
  end;

  procedure CreateEditorTime;
  begin
    FEditor:= TDateTimePicker.Create(FTree);
    TDateTimePicker(FEditor).Kind:= dtkTime;
    TDateTimePicker(FEditor).TimeDisplay:= tdHMSMs;
    TDateTimePicker(FEditor).TimeFormat:= tf24;
    TDateTimePicker(FEditor).BorderStyle:= bsNone;
    TDateTimePicker(FEditor).Alignment:=FTree.Header.Columns[FSelectedColIndex].Alignment;
    TDateTimePicker(FEditor).Time:= StrToTimeDef(SelectedText, 0);
  end;

begin
  if Assigned(FEditor) then FreeAndNil(FEditor);

  ColumnType:= FColumnTypes[FSelectedColIndex];
  case ColumnType of
    ctInteger: CreateEditorInteger;
    ctString:  CreateEditorString;
    ctDate:    CreateEditorDate;
    ctTime:    CreateEditorTime;
  else
    Exit;
  end;

  FEditor.OnKeyDown:= @EditorKeyDown;
  FEditor.Parent:= FTree;
  FEditor.AutoSize:= False;
  FEditor.BoundsRect:= SelectedCellRect;
  FEditor.Show;
  FEditor.SetFocus;
end;

procedure TVSTEdit.EndEdit;
var
  ColumnType: TVSTColumnType;
begin
  if not Assigned(FEditor) then Exit;

  ColumnType:= FColumnTypes[FSelectedColIndex];
  case ColumnType of
    ctInteger: if TSpinEdit(FEditor).Value=0 then
                 SelectedText:= EmptyStr
               else
                 SelectedText:= TSpinEdit(FEditor).Text;
    ctString:  SelectedText:= TEdit(FEditor).Text;
    ctDate:    SelectedText:= FormatDateTime(
                                    FColumnFormatStrings[FSelectedColIndex],
                                    TDateTimePicker(FEditor).Date);
    ctTime:    SelectedText:= FormatDateTime(
                                    FColumnFormatStrings[FSelectedColIndex],
                                    TDateTimePicker(FEditor).Time);
  end;
  FreeAndNil(FEditor);
end;

procedure TVSTEdit.SetRowTitlesColumnVisible(AValue: Boolean);
begin
  if not IsRowTitlesColumnExists then Exit;
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
  FTitleColumnIndex:= -1;
end;

procedure TVSTEdit.ValuesClear;
var
  i: Integer;
begin
  FSelectedRowIndex:= -1;
  FSelectedColIndex:= -1;
  for i:=0 to High(FDataValues) do
    FDataValues[i]:= nil;
  inherited ValuesClear;
end;

procedure TVSTEdit.Draw;
var
  i, n: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;

  if IsRowTitlesColumnExists then
  begin
    n:= Length(FDataValues[FTitleColumnIndex]);
    for i:= 0 to High(FDataValues) do
      if i<>FTitleColumnIndex then
        VReDim(FDataValues[i], n, EmptyStr);
    VSTLoad(FTree, FDataValues[FTitleColumnIndex]);
  end;

  SetColumnWidths;

end;

function TVSTEdit.SelectedCellRect: TRect;
var
  Node: PVirtualNode;
begin
  Result:= Rect(0,0,0,0);
  if not IsSelected then Exit;
  Node:= NodeFromIndex(FSelectedRowIndex);
  Result:= FTree.GetDisplayRect(Node, FSelectedColIndex, False);
  Result.Bottom:= Result.Bottom - 1;
end;

procedure TVSTEdit.AddRowTitlesColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment;
  const AFixColumn: Boolean = False);
begin
  if IsRowTitlesColumnExists then Exit;
  AddColumn(ACaption, AWidth, ACaptionAlignment);
  FTitleColumnIndex:= High(FHeaderCaptions);
  if AFixColumn then
    FTree.Header.Columns[FTitleColumnIndex].Options:=
      FTree.Header.Columns[FTitleColumnIndex].Options + [coFixed]
  else
    FTree.Header.Columns[FTitleColumnIndex].Options:=
      FTree.Header.Columns[FTitleColumnIndex].Options - [coFixed];
  SetNewColumn(ctUndefined, EmptyStr);
end;

procedure TVSTEdit.SetRowTitlesColumn(const AValues: TStrVector;
  const AValuesAlignment: TAlignment);
begin
  if not IsRowTitlesColumnExists then Exit;
  FDataValues[FTitleColumnIndex]:= VCut(AValues);
  FTree.Header.Columns[FTitleColumnIndex].Alignment:= AValuesAlignment;
end;

procedure TVSTEdit.SetNewColumn(const AColumnType: TVSTColumnType;
  const AFormatString: String);
begin
  MAppend(FDataValues, nil);
  VAppend(FColumnFormatStrings, AFormatString);
  SetLength(FColumnTypes, Length(FHeaderCaptions));
  FColumnTypes[High(FColumnTypes)]:= AColumnType;
end;

procedure TVSTEdit.AddIntegerColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment;
  const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctInteger, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddStringColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment;
  const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctString, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddDateColumn(const ACaption, AFormatString: String;
  const AWidth: Integer;
  const ACaptionAlignment: TAlignment; const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctDate, ACaption, AFormatString, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddTimeColumn(const ACaption, AFormatString: String;
  const AWidth: Integer;
  const ACaptionAlignment: TAlignment; const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctTime, ACaption, AFormatString, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddValuesColumn(const AColumnType: TVSTColumnType;
                             const ACaption, AFormatString: String;
                             const AWidth: Integer;
                             const ACaptionAlignment: TAlignment;
                             const AValuesAlignment: TAlignment);
begin
  AddColumn(ACaption, AWidth, ACaptionAlignment);
  FTree.Header.Columns[High(FHeaderCaptions)].Alignment:= AValuesAlignment;
  SetNewColumn(AColumnType, AFormatString);
end;

procedure TVSTEdit.UnSelect;
begin
  if IsSelected then
    UnselectCell;
end;

{ TVSTCategoryCheckTable }

function TVSTCategoryCheckTable.GetChecked(AIndex1, AIndex2: Integer): Boolean;
begin
  Result:= False;
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  Result:= FSelected[AIndex1, AIndex2];
end;

procedure TVSTCategoryCheckTable.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if FTree.GetNodeLevel(Node)=0 then
  begin
    Node^.CheckType:= ctTriStateCheckBox;
    Node^.CheckState:= csUncheckedNormal;
  end
  else if FTree.GetNodeLevel(Node)=1 then
  begin
    Node^.CheckType:= ctCheckBox;
    Node^.CheckState:= csUncheckedNormal;
  end;
end;

procedure TVSTCategoryCheckTable.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  Node:= FTree.GetNodeAt(X, Y);
  if not Assigned(Node) then Exit;

  if Button=mbRight then
  begin
    if FCanRightMouseButtonUnselect then
      CheckAll(False);
  end
  else if Button=mbLeft then
  begin
    if FTree.GetNodeLevel(Node)=1 then
    begin
      ReverseCheckState(Node);
      SetCategoryCheckState(Node^.Parent);
    end
    else if FTree.GetNodeLevel(Node)=0 then
      ReverseCategoryCheckState(Node);
  end;

  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.Checking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  if FTree.GetNodeLevel(Node) = 1 then
  begin
    if NewState=csUncheckedNormal then
    begin
      Check(Node);
      NewState:= csCheckedNormal;
    end
    else if NewState=csCheckedNormal then
    begin
      Uncheck(Node);
      NewState:= csUncheckedNormal;
    end;
    SetCategoryCheckState(Node^.Parent);
  end
  else if FTree.GetNodeLevel(Node) = 0 then
  begin
    if IsAllCheckedCategory(Node) then
    begin
      CheckCategory(Node, True);
      NewState:= csCheckedNormal;
    end
    else if IsAllUncheckedCategory(Node) then
    begin
      CheckCategory(Node, False);
      NewState:= csUncheckedNormal;
    end
    else begin
      if NewState=csUncheckedNormal then
      begin
        CheckCategory(Node, True);
        NewState:= csCheckedNormal;
      end
      else if NewState=csCheckedNormal then
      begin
        CheckCategory(Node, False);
        NewState:= csUncheckedNormal;
      end;
    end;
  end;
end;

procedure TVSTCategoryCheckTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then CheckAll(False);
  inherited SetCanSelect(AValue);
end;

procedure TVSTCategoryCheckTable.CheckNode(Node: PVirtualNode; const AChecked: Boolean);
begin
  if (not Assigned(Node)) or MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[(Node^.Parent)^.Index]) then Exit;
  if AChecked then
    Node^.CheckState:= csCheckedNormal
  else
    Node^.CheckState:= csUnCheckedNormal;
  FSelected[(Node^.Parent)^.Index, Node^.Index]:= AChecked;
  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.Check(Node: PVirtualNode);
begin
  CheckNode(Node, True);
end;

procedure TVSTCategoryCheckTable.Check(const AIndex1, AIndex2: Integer);
begin
  Check(NodeFromIndex(AIndex1, AIndex2));
end;

procedure TVSTCategoryCheckTable.SetChecked(AIndex1, AIndex2: Integer;
  AValue: Boolean);
begin
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  if Avalue then
    Check(AIndex1, AIndex2)
  else
    Uncheck(AIndex1, AIndex2);
end;

procedure TVSTCategoryCheckTable.Uncheck(Node: PVirtualNode);
begin
  CheckNode(Node, False);
end;

procedure TVSTCategoryCheckTable.Uncheck(const AIndex1, AIndex2: Integer);
begin
  Uncheck(NodeFromIndex(AIndex1, AIndex2));
end;

procedure TVSTCategoryCheckTable.ReverseCheckState(Node: PVirtualNode);
begin
  if Node^.CheckState=csUncheckedNormal then
    Check(Node)
  else if Node^.CheckState=csCheckedNormal then
    Uncheck(Node);
end;

procedure TVSTCategoryCheckTable.ReverseCategoryCheckState(Node: PVirtualNode);
begin
  if Node^.CheckState=csUncheckedNormal then
    CheckCategory(Node, True)
  else if Node^.CheckState=csCheckedNormal then
    CheckCategory(Node, False);
end;





constructor TVSTCategoryCheckTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnInitNode:= @InitNode;
  FTree.OnChecking:= @Checking;
end;

destructor TVSTCategoryCheckTable.Destroy;
begin
  inherited Destroy;
end;

function TVSTCategoryCheckTable.IsHasCheckedCategory(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsHasCheckedCategory(Node^.Index);
end;

function TVSTCategoryCheckTable.IsHasCheckedCategory(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[AIndex]) then Exit;
  Result:= VIsTrue(FSelected[AIndex]);
end;

function TVSTCategoryCheckTable.IsAllCheckedCategory(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[AIndex]) then Exit;
  Result:= VIsAllTrue(FSelected[AIndex]);
end;

function TVSTCategoryCheckTable.IsAllUncheckedCategory(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[AIndex]) then Exit;
  Result:= VIsAllFalse(FSelected[AIndex]);
end;

function TVSTCategoryCheckTable.IsAllCheckedCategory(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsAllCheckedCategory(Node^.Index);
end;

function TVSTCategoryCheckTable.IsAllUncheckedCategory(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsAllUncheckedCategory(Node^.Index);
end;



procedure TVSTCategoryCheckTable.CheckAll(const AChecked: Boolean);
var
  Node: PVirtualNode;
begin
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if FTree.GetNodeLevel(Node)=0 then
      CheckCategory(Node, AChecked);
    Node:= FTree.GetNext(Node);
  end;
  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.CheckCategory(Node: PVirtualNode;
  const AChecked: Boolean);
var
  Ind, i: Integer;
begin
  if not Assigned(Node) then Exit;
  if FTree.GetNodeLevel(Node)<>0 then Exit;
  Ind:= Node^.Index;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[Ind]) then Exit;
  for i:= 0 to High(FSelected[Ind]) do
  begin
    if AChecked then
    begin
      Check(Ind, i);
      Node^.CheckState:= csCheckedNormal
    end
    else begin
      Uncheck(Ind, i);
      Node^.CheckState:= csUnCheckedNormal;
    end;
  end;
end;

procedure TVSTCategoryCheckTable.SetCategoryCheckState(Node: PVirtualNode);
begin
  if IsAllCheckedCategory(Node) then
   Node^.CheckState:= csCheckedNormal
  else if IsAllUncheckedCategory(Node) then
    Node^.CheckState:= csUncheckedNormal
  else if IsHasCheckedCategory(Node) then
    Node^.CheckState:= csMixedNormal;
end;

procedure TVSTCategoryCheckTable.CheckCategory(const AIndex: Integer; const AChecked: Boolean);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(AIndex);
  if not Assigned(Node) then Exit;
  CheckCategory(Node, AChecked);
end;

{ TVSTCategoryRadioButtonTable }

function TVSTCategoryRadioButtonTable.GetSelectedIndex1: Integer;
var
  Ind1, Ind2: Integer;
begin
  SelectedIndexes(Ind1, Ind2);
  Result:= Ind1;
end;

function TVSTCategoryRadioButtonTable.GetSelectedIndex2: Integer;
var
  Ind1, Ind2: Integer;
begin
  SelectedIndexes(Ind1, Ind2);
  Result:= Ind2;
end;

procedure TVSTCategoryRadioButtonTable.InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if FTree.GetNodeLevel(Node)=1 then
  begin
    Node^.CheckType:= ctRadioButton;
    Node^.CheckState:= csUncheckedNormal;
  end;
end;

procedure TVSTCategoryRadioButtonTable.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  if not FCanSelect then Exit;

  Node:= FTree.GetNodeAt(X, Y);
  if not Assigned(Node) then Exit;

  if Button=mbRight then
  begin
    if FCanRightMouseButtonUnselect then
      UnselectNode;
  end
  else if Button=mbLeft then
  begin
    if FTree.GetNodeLevel(Node)=1 then
      SelectNode(Node);
  end;
end;

procedure TVSTCategoryRadioButtonTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then UnselectNode;
  inherited SetCanSelect(AValue);
end;

procedure TVSTCategoryRadioButtonTable.SelectNode(Node: PVirtualNode);
begin
  UnselectNode;
  FSelected[(Node^.Parent)^.Index, Node^.Index]:= True;
  FTree.FocusedNode:= Node;
  Node^.CheckState:= csCheckedNormal;
  FTree.Refresh;
end;

procedure TVSTCategoryRadioButtonTable.UnselectNode;
var
  Node: PVirtualNode;
  i, j: Integer;
begin
  if not IsSelected then Exit;

  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if FTree.GetNodeLevel(Node)=1 then
      Node^.CheckState:= csUncheckedNormal;
    Node:= FTree.GetNext(Node);
  end;

  SelectedIndexes(i,j);
  FSelected[i,j]:= False;

  FTree.Refresh;
end;

constructor TVSTCategoryRadioButtonTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.OnInitNode:= @InitNode;
  FTree.OnMouseDown:= @MouseDown;
end;

destructor TVSTCategoryRadioButtonTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTCategoryRadioButtonTable.Draw;
begin
  UnselectNode;
  inherited Draw;
  FTree.Refresh;
end;

procedure TVSTCategoryRadioButtonTable.ValuesClear;
begin
  UnselectNode;
  inherited ValuesClear;
end;

procedure TVSTCategoryRadioButtonTable.Select(const AIndex1, AIndex2: Integer);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(AIndex1, AIndex2);
  if Assigned(Node) then SelectNode(Node);
end;

procedure TVSTCategoryRadioButtonTable.SelectedIndexes(out AIndex1, AIndex2: Integer);
begin
  MIndexOf(FSelected, True, AIndex1, AIndex2);
end;

{ TVSTCategoryCustomTable }

procedure TVSTCategoryCustomTable.SetTreeLinesVisible(AValue: Boolean);
begin
  if Avalue then
    FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions + [toShowTreeLines]
  else
    FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
end;

procedure TVSTCategoryCustomTable.HeaderClear;
begin
  inherited HeaderClear;
  FDataValues:= nil;
end;

procedure TVSTCategoryCustomTable.GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  i, j, k: Integer;
begin
  CellText:= EmptyStr;
  if FTree.GetNodeLevel(Node)=0 then
  begin
    if Column=0 then
    begin
      i:= Node^.Index;
      CellText:= FCategoryValues[i];
    end;
  end
  else begin
    if Column>=0 then
    begin
      i:= Column;
      j:= (Node^.Parent)^.Index;
      k:= Node^.Index;
      CellText:= FDataValues[i,j,k];
    end;
  end;
end;

function TVSTCategoryCustomTable.IsIndexesCorrect(const AIndex1,AIndex2: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FDataValues) then Exit;
  if MIsNil(FDataValues[0]) then Exit;
  if (AIndex1>=0) and (AIndex1<=High(FDataValues[0])) then
  begin
    if VIsNil(FDataValues[0,AIndex1]) then Exit;
    Result:= (AIndex2>=0) and (AIndex2<=High(FDataValues[0,AIndex1]))
  end;
end;

function TVSTCategoryCustomTable.IsCellSelected(Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  Result:= False;
  if FTree.GetNodeLevel(Node)<>1 then Exit;
  Result:= FSelected[(Node^.Parent)^.Index, Node^.Index];
end;

function TVSTCategoryCustomTable.GetIsSelected: Boolean;
var
  Ind1, Ind2: Integer;
begin
  MIndexOf(FSelected, True, Ind1, Ind2);
  Result:= (Ind1>=0) and (Ind2>=0);
end;

constructor TVSTCategoryCustomTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  Clear;
  FTree.Margin:= 0;
  FCanSelect:= True;
  FSelectedBGColor:= FTree.Color;
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toCheckSupport];
  FTree.TreeOptions.AutoOptions:= FTree.TreeOptions.AutoOptions + [toAutoSpanColumns];
  FTree.LineStyle:= lsSolid;
  FTree.OnGetText:= @GetText;
end;

destructor TVSTCategoryCustomTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTCategoryCustomTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
begin
  inherited AddColumn(ACaption, AWidth, ACaptionAlignment);
  MAppend(FDataValues, nil);
end;

procedure TVSTCategoryCustomTable.SetColumn(const AColumnIndex: Integer;
  const AValues: TStrMatrix; const AValuesAlignment: TAlignment);
begin
  FDataValues[AColumnIndex]:= MCut(AValues);
  FTree.Header.Columns[AColumnIndex].Alignment:= AValuesAlignment;
end;

procedure TVSTCategoryCustomTable.SetColumn(const ACaption: String;
  const AValues: TStrMatrix; const AValuesAlignment: TAlignment);
var
  ColumnIndex: Integer;
begin
  ColumnIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColumnIndex>=0 then
    SetColumn(ColumnIndex, AValues, AValuesAlignment);
end;

procedure TVSTCategoryCustomTable.SetCategories(const AValues: TStrVector);
begin
  FCategoryValues:= VCut(AValues);
end;

procedure TVSTCategoryCustomTable.Clear;
begin
  ValuesClear;
  HeaderClear;
end;

procedure TVSTCategoryCustomTable.ValuesClear;
var
  i: Integer;
begin
  FCategoryValues:= nil;
  FSelected:= nil;
  for i:=0 to High(FDataValues) do
    FDataValues[i]:= nil;
  inherited ValuesClear;
end;

procedure TVSTCategoryCustomTable.Draw;
var
  ColumnIndex, CategoryIndex, MaxLength: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;
  if MIsNil(FDataValues) then Exit;

  MaxLength:= 0;
  for ColumnIndex:= 0 to High(FHeaderCaptions) do
  begin
    if Length(FDataValues[ColumnIndex])>MaxLength then
      MaxLength:= Length(FDataValues[ColumnIndex]);
  end;
  for ColumnIndex:= 0 to High(FHeaderCaptions) do
    MReDim(FDataValues[ColumnIndex], MaxLength);

  VReDim(FCategoryValues, MaxLength, EmptyStr);

  for CategoryIndex:= 0 to High(FCategoryValues) do
  begin
    MaxLength:= 0;
    for ColumnIndex:= 0 to High(FHeaderCaptions) do
    begin
      if Length(FDataValues[ColumnIndex, CategoryIndex])>MaxLength then
        MaxLength:= Length(FDataValues[ColumnIndex, CategoryIndex]);
    end;
    for ColumnIndex:= 0 to High(FHeaderCaptions) do
      VReDim(FDataValues[ColumnIndex, CategoryIndex], MaxLength);
  end;

  MReDim(FSelected, Length(FCategoryValues));
  for CategoryIndex:= 0 to High(FCategoryValues) do
    VReDim(FSelected[CategoryIndex], Length(FDataValues[0,CategoryIndex]));

  VSTLoad(FTree, FDataValues[0], False);

  SetColumnWidths;

end;

procedure TVSTCategoryCustomTable.ExpandAll(const AExpand: Boolean);
var
  Node: PVirtualNode;
begin
  if MIsNil(FDataValues) then Exit;
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if FTree.GetNodeLevel(Node)=0 then
      FTree.Expanded[Node]:= AExpand;
    Node:= FTree.GetNext(Node);
  end;
end;

procedure TVSTCategoryCustomTable.Show(const AIndex1, AIndex2: Integer);
var
  Node: PVirtualNode;
begin
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  Node:= NodeFromIndex(AIndex1, AIndex2);
  if not Assigned(Node) then Exit;
  FTree.Expanded[Node^.Parent]:= True;
  FTree.FocusedNode:= Node;
end;

{ TVSTCoreTable }

procedure TVSTCoreTable.SetHeaderVisible(AValue: Boolean);
begin
  if AValue then
    FTree.Header.Options:= FTree.Header.Options + [hoVisible]
  else
    FTree.Header.Options:= FTree.Header.Options - [hoVisible];
end;

procedure TVSTCoreTable.SetGridLinesColor(AValue: TColor);
begin
  if FGridLinesColor=AValue then Exit;
  FGridLinesColor:=AValue;
  FTree.Colors.GridLineColor:= FGridLinesColor;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetGridLinesVisible(AValue: Boolean);
begin
  if FGridLinesVisible=AValue then Exit;
  FGridLinesVisible:=AValue;
  FTree.Refresh;
end;

function TVSTCoreTable.NodeFromIndex(const AIndex: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result:= nil;
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if FTree.GetNodeLevel(Node)=0 then
    begin
      if (Node^.Index = AIndex) then
      begin
        Result:= Node;
        break;
      end;
    end;
    Node:= FTree.GetNext(Node);
  end;
end;

function TVSTCoreTable.NodeFromIndex(const AIndex1, AIndex2: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result:= nil;
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if (FTree.GetNodeLevel(Node)=1) then
    begin
      if ((Node^.Parent)^.Index=AIndex1) and (Node^.Index = AIndex2) then
      begin
        Result:= Node;
        break;
      end;
    end;
    Node:= FTree.GetNext(Node);
  end;
end;

procedure TVSTCoreTable.SetCanSelect(AValue: Boolean);
begin
  if FCanSelect=AValue then Exit;
  FCanSelect:=AValue;
end;

procedure TVSTCoreTable.HeaderClear;
begin
  FHeaderCaptions:= nil;
  FColumnWidths:= nil;
  FTree.Header.Columns.Clear;

end;

procedure TVSTCoreTable.SetColumnWidths;
var
  i: Integer;
begin
  FTree.Header.AutoSizeIndex:= -1;
  if FAutosizeColumnIndex>=0 then
  begin
    if FAutosizeColumnIndex<=High(FHeaderCaptions) then
    FTree.Header.AutoSizeIndex:= FAutosizeColumnIndex;
  end
  else if FAutosizeColumnIndex=-2 then
    FTree.Header.AutoSizeIndex:= High(FHeaderCaptions);
  for i:= 0 to High(FHeaderCaptions) do
    FTree.Header.Columns[i].Width:= FColumnWidths[i];
end;

procedure TVSTCoreTable.SetHeaderBGColor(AValue: TColor);
begin
  if FHeaderBGColor=AValue then Exit;
  FHeaderBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetHeaderFont(AValue: TFont);
begin
  FHeaderFont.Assign(AValue);
  FTree.Header.Font.Assign(FHeaderFont);
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetSelectedBGColor(AValue: TColor);
begin
  if FSelectedBGColor=AValue then Exit;
  FSelectedBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetSelectedFont(AValue: TFont);
begin
  FSelectedFont.Assign(AValue);
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetValuesBGColor(AValue: TColor);
begin
  if FValuesBGColor=AValue then Exit;
  FValuesBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetValuesFont(AValue: TFont);
begin
  FValuesFont.Assign(AValue);
  FTree.Font.Assign(FValuesFont);
  FTree.Refresh;
end;

procedure TVSTCoreTable.HeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements:= [hpeBackground];
end;

procedure TVSTCoreTable.AdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  PaintInfo.TargetCanvas.Font.Assign(FHeaderFont);

  if (not Assigned(PaintInfo.Column)) or VIsNil(FHeaderCaptions) then
    VSTHeaderDraw(FTree.Color, FTree.Color, PaintInfo, Elements)
  else begin
    if FGridLinesVisible then
      VSTHeaderDraw(FGridLinesColor, FHeaderBGColor, PaintInfo, Elements)
    else
      VSTHeaderDraw(FHeaderBGColor, FHeaderBGColor, PaintInfo, Elements);
  end;
end;

procedure TVSTCoreTable.BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  BGColor: TColor;
  NeedTopLine: Boolean;
begin
  BGColor:= CellBGColor(Node, Column);
  NeedTopLine:= (Sender.GetNodeLevel(Node)=0) and (Node^.Index=0) and
                (not (hoVisible in FTree.Header.Options));
  if FGridLinesVisible then
    VSTCellDraw(FGridLinesColor, BGColor, TargetCanvas, Column, CellRect, NeedTopLine)
  else
    VSTCellDraw(BGColor, BGColor, TargetCanvas, Column, CellRect, NeedTopLine);
end;

procedure TVSTCoreTable.DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  TargetCanvas.Font.Assign(CellFont(Node, Column));
end;

function TVSTCoreTable.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= Assigned(Node) and (Column>=0) and (Column<=High(FHeaderCaptions));
end;

function TVSTCoreTable.CellBGColor(Node: PVirtualNode; Column: TColumnIndex): TColor;
begin
  if IsCellSelected(Node, Column) then
    Result:= FSelectedBGColor
  else
    Result:= FValuesBGColor;
end;

function TVSTCoreTable.CellFont(Node: PVirtualNode; Column: TColumnIndex): TFont;
begin
  if IsCellSelected(Node, Column) then
    Result:= FSelectedFont
  else
    Result:= FValuesFont;
end;

constructor TVSTCoreTable.Create(const ATree: TVirtualStringTree);
begin
  FTree:= ATree;

  Clear;

  FHeaderFont:= TFont.Create;
  FValuesFont:= TFont.Create;
  FSelectedFont:= TFont.Create;
  FHeaderFont.Assign(FTree.Header.Font);
  FValuesFont.Assign(FTree.Font);
  FSelectedFont.Assign(FTree.Font);

  FGridLinesVisible:= True;
  FGridLinesColor:= clWindowText;
  FValuesBGColor:= clWindow;
  FHeaderBGColor:= FValuesBGColor;
  FSelectedBGColor:= clHighlight;

  FCanRightMouseButtonUnselect:= True;

  FTree.Colors.GridLineColor:= FGridLinesColor;
  FTree.Color:= FValuesBGColor;

  FTree.DefaultNodeHeight:= 25;
  FTree.Header.DefaultHeight:= FTree.DefaultNodeHeight + 1;

  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions +
                                   [toAlwaysHideSelection, toHideFocusRect];

  FTree.Header.Options:= FTree.Header.Options + [hoOwnerDraw, hoVisible];
  AutosizeColumnEnable(-2); //last column

  FTree.OnHeaderDrawQueryElements:= @HeaderDrawQueryElements;
  FTree.OnAdvancedHeaderDraw:= @AdvancedHeaderDraw;
  FTree.OnBeforeCellPaint:= @BeforeCellPaint;
  FTree.OnDrawText:= @DrawText;
end;

destructor TVSTCoreTable.Destroy;
begin
  FreeAndNil(FHeaderFont);
  FreeAndNil(FValuesFont);
  FreeAndNil(FSelectedFont);
  inherited Destroy;
end;

procedure TVSTCoreTable.ValuesClear;
begin
  FTree.Clear;
end;

procedure TVSTCoreTable.Clear;
begin
  ValuesClear;
  HeaderClear;
end;

procedure TVSTCoreTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
var
  Col: TVirtualTreeColumn;
begin
  VAppend(FHeaderCaptions, ACaption);
  VAppend(FColumnWidths, AWidth);
  Col:= FTree.Header.Columns.Add;
  Col.Text:= ACaption;
  Col.CaptionAlignment:= ACaptionAlignment;
  Col.Margin:= 3;
  Col.Spacing:= 0;
  Col.Width:= AWidth;
end;

procedure TVSTCoreTable.AutosizeColumnEnable(const AColumnIndex: Integer);
begin
  if AColumnIndex<0 then
    if AColumnIndex<>-2 then
      Exit;
  FAutosizeColumnIndex:= AColumnIndex;
  FTree.Header.Options:= FTree.Header.Options + [hoAutoResize];
  FTree.ScrollBarOptions.ScrollBars:= ssVertical;
  SetColumnWidths;
end;

procedure TVSTCoreTable.AutosizeColumnDisable;
begin
  FAutosizeColumnIndex:= -1;
  FTree.Header.Options:= FTree.Header.Options - [hoAutoResize];
  FTree.ScrollBarOptions.ScrollBars:= ssBoth;
  SetColumnWidths;
end;

{ TVSTCheckTable }

function TVSTCheckTable.GetIsAllUnchecked: Boolean;
begin
  Result:= VIsAllFalse(FSelected);
end;

procedure TVSTCheckTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then CheckAll(False);
  inherited SetCanSelect(AValue);
end;

function TVSTCheckTable.GetIsAllChecked: Boolean;
begin
  Result:= VIsAllTrue(FSelected);
end;

function TVSTCheckTable.GetChecked(AIndex: Integer): Boolean;
begin
  Result:= False;
  if not IsIndexCorrect(AIndex) then Exit;
  Result:= FSelected[AIndex];
end;

procedure TVSTCheckTable.SetChecked(AIndex: Integer; AValue: Boolean);
begin
  if not IsIndexCorrect(AIndex) then Exit;
  if Avalue then
    Check(AIndex)
  else
    Uncheck(AIndex);
end;

procedure TVSTCheckTable.ReverseCheckState(Node: PVirtualNode);
begin
  if Node^.CheckState=csUncheckedNormal then
    Check(Node)
  else if Node^.CheckState=csCheckedNormal then
    Uncheck(Node);
end;

procedure TVSTCheckTable.CheckNode(Node: PVirtualNode; const AChecked: Boolean);
begin
  if (not Assigned(Node)) or VIsNil(FSelected) then Exit;
  if AChecked then
    Node^.CheckState:= csCheckedNormal
  else
    Node^.CheckState:= csUnCheckedNormal;
  FSelected[Node^.Index]:= AChecked;
  FTree.Refresh;
end;

procedure TVSTCheckTable.Check(Node: PVirtualNode);
begin
  CheckNode(Node, True);
end;

procedure TVSTCheckTable.Uncheck(Node: PVirtualNode);
begin
  CheckNode(Node, False);
end;

procedure TVSTCheckTable.CheckAll(const AChecked: Boolean);
var
  Node: PVirtualNode;
begin
  if MIsNil(FDataValues) then Exit;
  if VIsNil(FDataValues[0]) then Exit;
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if AChecked then
      Check(Node)
    else
      Uncheck(Node);
    FSelected[Node^.Index]:= AChecked;
    Node:= FTree.GetNext(Node);
  end;
  FTree.Refresh;
end;

procedure TVSTCheckTable.Check(const AIndex: Integer);
begin
  Check(NodeFromIndex(AIndex));
end;

procedure TVSTCheckTable.Uncheck(const AIndex: Integer);
begin
  Uncheck(NodeFromIndex(AIndex));
end;

function TVSTCheckTable.GetSelected: TBoolVector;
begin
  Result:= VCut(FSelected);
end;

procedure TVSTCheckTable.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Ind: Integer;
  Node: PVirtualNode;
begin
  Node:= FTree.GetNodeAt(X, Y);
  if not Assigned(Node) then Exit;
  Ind:= Node^.Index;
  if not IsIndexCorrect(Ind) then Exit;

  if Button=mbRight then
  begin
    if FCanRightMouseButtonUnselect then
      CheckAll(False)
  end
  else if Button=mbLeft then
    ReverseCheckState(Node);
  FTree.Refresh;
end;

procedure TVSTCheckTable.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType:= ctCheckBox;
  Node^.CheckState:= csUncheckedNormal;
end;

procedure TVSTCheckTable.Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  if NewState=csUncheckedNormal then
  begin
    Check(Node);
    NewState:= csCheckedNormal;
  end
  else if NewState=csCheckedNormal then
  begin
    Uncheck(Node);
    NewState:= csUncheckedNormal;
  end;
end;

constructor TVSTCheckTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toCheckSupport];


  FTree.OnMouseDown:= @MouseDown;
  FTree.OnInitNode:= @InitNode;
  FTree.OnChecking:= @Checking;
end;

destructor TVSTCheckTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTCheckTable.ValuesClear;
begin
  FSelected:= nil;
  inherited ValuesClear;
end;

procedure TVSTCheckTable.Draw;
begin
  inherited Draw;
  if not VIsNil(FHeaderCaptions) then
    FTree.Header.Columns[0].CheckType:= ctCheckBox;
  CheckAll(False);
  FTree.Refresh;
end;

{ TVSTCustomTable }

function TVSTCustomTable.GetIsSelected: Boolean;
begin
  Result:= VIsTrue(FSelected);
end;

function TVSTCustomTable.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  Result:= FSelected[Node^.Index];
end;

procedure TVSTCustomTable.HeaderClear;
begin
  inherited HeaderClear;
  FDataValues:= nil;
end;

procedure TVSTCustomTable.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  i: Integer;
begin
  if High(FDataValues)<Column then Exit;
  i:= Node^.Index;
  CellText:= EmptyStr;
  if not VIsNil(FDataValues[Column]) then
    CellText:= FDataValues[Column, i];
end;

function TVSTCustomTable.IsIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FDataValues) then Exit;
  if VIsNil(FDataValues[0]) then Exit;
  Result:= (AIndex>=0) and (AIndex<=High(FDataValues[0]));
end;

constructor TVSTCustomTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.Indent:= 0;
  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
  FTree.OnGetText:= @GetText;
end;

destructor TVSTCustomTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTCustomTable.ValuesClear;
var
  i: Integer;
begin
  FSelected:= nil;
  for i:=0 to High(FDataValues) do
    FDataValues[i]:= nil;
  inherited ValuesClear;
end;

procedure TVSTCustomTable.Draw;
var
  i, MaxLength: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;

  MaxLength:= MMaxLength(FDataValues);
  VDim(FSelected, MaxLength, False);
  for i:= 0 to High(FDataValues) do
    if Length(FDataValues[i])<MaxLength then
      VReDim(FDataValues[i], MaxLength, EmptyStr);

  VSTLoad(FTree, FDataValues[0]);
  SetColumnWidths;
end;

procedure TVSTCustomTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
begin
  inherited AddColumn(ACaption, AWidth, ACaptionAlignment);
  MAppend(FDataValues, nil);
end;

procedure TVSTCustomTable.SetColumn(const AColumnIndex: Integer;
  const AValues: TStrVector; const AValuesAlignment: TAlignment);
begin
  FDataValues[AColumnIndex]:= VCut(AValues);
  FTree.Header.Columns[AColumnIndex].Alignment:= AValuesAlignment;
end;

procedure TVSTCustomTable.SetColumn(const ACaption: String;
  const AValues: TStrVector; const AValuesAlignment: TAlignment);
var
  ColumnIndex: Integer;
begin
  ColumnIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColumnIndex>=0 then
    SetColumn(ColumnIndex, AValues, AValuesAlignment);
end;

procedure TVSTCustomTable.Show(const AIndex: Integer);
var
  Node: PVirtualNode;
begin
  if not IsIndexCorrect(AIndex) then Exit;
  Node:= NodeFromIndex(AIndex);
  if not Assigned(Node) then Exit;
  FTree.FocusedNode:= Node;
end;

{ TVSTTable }

procedure TVSTTable.Select(const AIndex: Integer);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(AIndex);
  if Assigned(Node) then SelectNode(Node);
end;

procedure TVSTTable.Select(const AColumnIndex: Integer; const AValue: String);
var
  Ind: Integer;
begin
  Ind:= VIndexOf(FDataValues[AColumnIndex], AValue);
  if Ind>=0 then
    Select(Ind);
end;

procedure TVSTTable.Select(const AColumnCaption, AValue: String);
var
  Ind: Integer;
begin
  Ind:= VIndexOf(FHeaderCaptions, AColumnCaption);
  if Ind>=0 then
    Select(Ind, AValue);
end;

procedure TVSTTable.SelectNode(Node: PVirtualNode);
begin
  UnselectNode;
  FSelected[Node^.Index]:= True;
  FTree.FocusedNode:= Node;
  FTree.Refresh;
end;

procedure TVSTTable.UnselectNode;
begin
  if not IsSelected then Exit;
  FSelected[SelectedIndex]:= False;
  FTree.Refresh;
end;

procedure TVSTTable.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Ind: Integer;
  Node: PVirtualNode;
begin
  if not FCanSelect then Exit;

  Node:= FTree.GetNodeAt(X, Y);
  if not Assigned(Node) then Exit;
  Ind:= Node^.Index;
  if not IsIndexCorrect(Ind) then Exit;

  if Button=mbRight then
  begin
    if FCanRightMouseButtonUnselect then
      UnselectNode;
  end
  else if Button=mbLeft then
    SelectNode(Node);
end;

procedure TVSTTable.MoveSelection(const ADeltaIndex: Integer);
var
  Ind: Integer;
begin
  if (not IsSelected) or (ADeltaIndex=0) then Exit;

  Ind:= SelectedIndex+ADeltaIndex;
  if not IsIndexCorrect(Ind) then Exit;

  Select(Ind);

end;

procedure TVSTTable.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_UP then
    MoveSelection(-1)
  else if Key=VK_DOWN then
    MoveSelection(1);
end;

procedure TVSTTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then UnselectNode;
  inherited SetCanSelect(AValue);
end;

function TVSTTable.GetSelectedIndex: Integer;
begin
  Result:= VIndexOf(FSelected, True);
end;

constructor TVSTTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.Margin:= 0;
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions - [toCheckSupport];
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnKeyDown:= @KeyDown;
end;

destructor TVSTTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTTable.ValuesClear;
begin
  UnselectNode;
  inherited ValuesClear;
end;

procedure TVSTTable.Draw;
begin
  //UnselectNode;
  inherited Draw;
  //FTree.Refresh;
end;

procedure TVSTTable.UnSelect;
begin
  if IsSelected then
    UnselectNode;
end;



end.

