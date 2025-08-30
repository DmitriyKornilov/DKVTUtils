 unit DK_VSTTables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, LCLType, VirtualTrees, fpstypes,

  DK_VSTCore, DK_VSTTypes, DK_Vector, DK_Matrix, DK_Const, DK_Color,
  DK_SheetExporter, DK_SheetWriter;

type

  { TVSTCoreTable }

  TVSTCoreTable = class (TVSTCore)
  protected
    FDataValues: TStrMatrix;
    FSelected: TBoolVector;
    FVisibles: TBoolVector;

    FAutoHeight: Boolean;
    FMaxAutoHeightRowCount: Integer;

    function GetAutoHeightValue: Integer;
    function GetRowCount: Integer;
    function GetIsSelected: Boolean;

    procedure SetVisibles(const AValues: TBoolVector);
    function GetVisibles: TBoolVector;

    procedure HeaderClear; override;
    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    function IsIndexCorrect(const AIndex: Integer): Boolean;

    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);

    procedure Draw; virtual;
    procedure ValuesClear; override;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter); override;
    procedure SetColumn(const AColIndex: Integer; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetColumn(const ACaption: String; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);

    procedure Show(const AIndex: Integer);

    procedure AutoHeightUpdate;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight;
    property AutoHeightValue: Integer read GetAutoHeightValue;
    property MaxAutoHeightRowCount: Integer read FMaxAutoHeightRowCount write FMaxAutoHeightRowCount;
    property RowCount: Integer read GetRowCount;
    property IsSelected: Boolean read GetIsSelected;

    property Visibles: TBoolVector read GetVisibles write SetVisibles;
  end;

  { TVSTTable }

  TVSTTable = class(TVSTCoreTable)
  protected
    FAutosizeRowHeights: Boolean;
    FOnDelKeyDown: TVSTEvent;
    FOnReturnKeyDown: TVSTEvent;

    procedure SelectNode(Node: PVirtualNode);
    procedure UnselectNode;

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure KeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                          Node: PVirtualNode; var NodeHeight: Integer);

    procedure SetCanSelect(const AValue: Boolean); override;
    function GetSelectedIndex: Integer;

    procedure MoveSelection(const ADeltaIndex: Integer);

    procedure SetAutosizeRowHeights(const AValue: Boolean);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    procedure ValuesClear; override;
    procedure Draw; override;

    procedure UnSelect;
    procedure Select(const AIndex: Integer);
    procedure Select(const AColIndex: Integer; const AValue: String);
    procedure Select(const AColumnCaption, AValue: String);
    property SelectedIndex: Integer read GetSelectedIndex;
    function ReSelect(const AIDVector: TIntVector; const AIDValue: Integer;
                      const AFirstRowSelectIfNotFound: Boolean = False): Boolean;
    function ReSelect(const AIDVector: TDateVector; const AIDValue: TDate;
                      const AFirstRowSelectIfNotFound: Boolean = False): Boolean;

    procedure Save(const AColumnTypes: TVSTColumnTypes;
                   const ASheetName: String = 'Лист1';
                   const ADoneMessage: String = 'Выполнено!';
                   const ALandscape: Boolean = False);

    property AutosizeRowHeights: Boolean read FAutosizeRowHeights write SetAutosizeRowHeights;
    property OnDelKeyDown: TVSTEvent read FOnDelKeyDown write FOnDelKeyDown;
    property OnReturnKeyDown: TVSTEvent read FOnReturnKeyDown write FOnReturnKeyDown;
  end;

  { TVSTCheckTable }

  TVSTCheckTable = class(TVSTCoreTable)
  protected
    FOnCheck: TVSTRowCheckEvent;
    FMaxCheckedCount: Integer;
    //FStopSelectEventWhileCheckAll=True - OnSelect вызывается только после заверешения CheckAll
    //FStopSelectEventWhileCheckAll=False - OnSelect вызывается на изменение каждой позиции (default)
    FStopSelectEventWhileCheckAll: Boolean;
    FCanDoSelectEvent: Boolean;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure Checking(Sender: TBaseVirtualTree; {%H-}Node: PVirtualNode;
      var {%H-}NewState: TCheckState; var Allowed: Boolean);

    function GetIsAllChecked: Boolean;
    function GetIsAllUnchecked: Boolean;
    procedure SetMaxCheckedCount(const AValue: Integer);

    procedure SetCanSelect(const AValue: Boolean); override;

    procedure SetChecked(const AIndex: Integer; const AValue: Boolean);
    function GetChecked(const AIndex: Integer): Boolean;

    function GetCheckeds: TBoolVector;
    procedure SetCheckeds(const AValue: TBoolVector);

    function GetCheckedCount: Integer;
    function GetUncheckedCount: Integer;

    procedure CheckNode(Node: PVirtualNode; const AChecked: Boolean);
    procedure Check(const AIndex: Integer; const AChecked: Boolean);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    procedure ValuesClear; override;
    procedure Draw; override;

    procedure CheckAll(const AChecked: Boolean);

    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
    property Checkeds: TBoolVector read GetCheckeds write SetCheckeds;
    property IsAllChecked: Boolean read GetIsAllChecked;
    property IsAllUnchecked: Boolean read GetIsAllUnchecked;

    property CheckedCount: Integer read GetCheckedCount;
    property UncheckedCount: Integer read GetUncheckedCount;
    property MaxCheckedCount: Integer read FMaxCheckedCount write SetMaxCheckedCount;
    procedure MaxCheckedCountClear;

    property StopSelectEventWhileCheckAll: Boolean read FStopSelectEventWhileCheckAll write FStopSelectEventWhileCheckAll;
    property OnCheck: TVSTRowCheckEvent read FOnCheck write FOnCheck;
  end;

implementation

{ TVSTCoreTable }

procedure TVSTCoreTable.SetVisibles(const AValues: TBoolVector);
var
  i: Integer;
  Node: PVirtualNode;
begin
  if VIsNil(AValues) then Exit;
  FVisibles:= VCut(AValues);
  for i:= 0 to High(AValues) do
  begin
    Node:= NodeFromIndex(i);
    if not Assigned(Node) then continue;
    if AValues[i] then
      Node^.NodeHeight:= FRowHeight
    else
      Node^.NodeHeight:= 0;
  end;
  AutoHeightUpdate;
end;

function TVSTCoreTable.GetVisibles: TBoolVector;
begin
  Result:= VCut(FVisibles);
end;

procedure TVSTCoreTable.AutoHeightUpdate;
var
  H: Integer;
begin
  if AutoHeight and
    (FTree.Align<>alLeft)   and
    (FTree.Align<>alRight)  and
    (FTree.Align<>alClient) then
  begin
    FTree.ScrollBarOptions.ScrollBars:= ssNone;
    H:= AutoHeightValue;
    while FTree.Height<>H do
      FTree.Height:= H;
  end;
end;

function TVSTCoreTable.GetAutoHeightValue: Integer;
var
  N: Integer;
begin
  N:= VCountIf(FVisibles, True);
  if N=0 then N:= 1;
  if (MaxAutoHeightRowCount>0) and (N>MaxAutoHeightRowCount) then
    N:= MaxAutoHeightRowCount;
  Result:= FHeaderHeight*Ord(FHeaderVisible) + N*FRowHeight;
end;

function TVSTCoreTable.GetRowCount: Integer;
begin
  Result:= MMaxLength(FDataValues);
end;

constructor TVSTCoreTable.Create(const ATree: TVirtualStringTree;
  const AHeaderHeight: Integer; const ARowHeight: Integer);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FAutoHeight:= False;
  FMaxAutoHeightRowCount:= 0;
  FTree.Indent:= 0;
  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
  FTree.OnGetText:= @GetText;
end;

procedure TVSTCoreTable.Draw;
var
  i, MaxLength: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;

  MaxLength:= MMaxLength(FDataValues);
  VDim(FSelected, MaxLength, False);
  VReDim(FVisibles, MaxLength, True);
  for i:= 0 to High(FDataValues) do
    if Length(FDataValues[i])<MaxLength then
      VReDim(FDataValues[i], MaxLength, EmptyStr);

  VSTLoad(FTree, FDataValues[0]);

  SetColumnWidths;
  AutoHeightUpdate;
end;

procedure TVSTCoreTable.ValuesClear;
var
  i: Integer;
begin
  FSelected:= nil;
  for i:=0 to High(FDataValues) do
    FDataValues[i]:= nil;
  inherited ValuesClear;
end;

function TVSTCoreTable.GetIsSelected: Boolean;
begin
  Result:= False;
  if not Assigned(Self) then Exit;
  Result:= VIsTrue(FSelected);
end;

function TVSTCoreTable.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  if not VIsNil(FSelected) then
    Result:= FSelected[Node^.Index];
end;

procedure TVSTCoreTable.HeaderClear;
begin
  inherited HeaderClear;
  FDataValues:= nil;
  FColumnVisibles:= nil;
end;

procedure TVSTCoreTable.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
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

function TVSTCoreTable.IsIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FDataValues) then Exit;
  if VIsNil(FDataValues[0]) then Exit;
  Result:= (AIndex>=0) and (AIndex<=High(FDataValues[0]));
end;

procedure TVSTCoreTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
begin
  inherited AddColumn(ACaption, AWidth, ACaptionAlignment);
  MAppend(FDataValues, nil);
  VAppend(FColumnVisibles, True);
end;

procedure TVSTCoreTable.SetColumn(const AColIndex: Integer;
  const AValues: TStrVector; const AValuesAlignment: TAlignment);
begin
  FDataValues[AColIndex]:= VCut(AValues);
  FTree.Header.Columns[AColIndex].Alignment:= AValuesAlignment;
end;

procedure TVSTCoreTable.SetColumn(const ACaption: String;
  const AValues: TStrVector; const AValuesAlignment: TAlignment);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColIndex>=0 then
    SetColumn(ColIndex, AValues, AValuesAlignment);
end;

procedure TVSTCoreTable.Show(const AIndex: Integer);
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
  if not IsIndexCorrect(AIndex) then Exit;
  Node:= NodeFromIndex(AIndex);
  if Assigned(Node) then SelectNode(Node);
end;

procedure TVSTTable.Select(const AColIndex: Integer; const AValue: String);
var
  Ind: Integer;
begin
  Ind:= VIndexOf(FDataValues[AColIndex], AValue);
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

function TVSTTable.ReSelect(const AIDVector: TIntVector; const AIDValue: Integer;
                            const AFirstRowSelectIfNotFound: Boolean = False): Boolean;
var
  Index: Integer;
begin
  Result:= False;
  if VIsNil(AIDVector) then Exit;

  Index:= -1;
  if AIDValue<=0 then
  begin
    if AFirstRowSelectIfNotFound then
      Index:= 0;
  end
  else begin
    Index:= VIndexOf(AIDVector, AIDValue);
    if (Index<0) and AFirstRowSelectIfNotFound then
      Index:= 0;
  end;

  if Index>=0 then
  begin
    Show(Index);
    Select(Index);
    Result:= True;
  end;
end;

function TVSTTable.ReSelect(const AIDVector: TDateVector; const AIDValue: TDate;
                            const AFirstRowSelectIfNotFound: Boolean = False): Boolean;
var
  Index: Integer;
begin
  Result:= False;
  if VIsNil(AIDVector) then Exit;

  Index:= -1;
  if AIDValue<=0 then
  begin
    if AFirstRowSelectIfNotFound then
      Index:= 0;
  end
  else begin
    Index:= VIndexOfDate(AIDVector, AIDValue);
    if (Index<0) and AFirstRowSelectIfNotFound then
      Index:= 0;
  end;

  if Index>=0 then
  begin
    Show(Index);
    Select(Index);
    Result:= True;
  end;
end;

procedure TVSTTable.Save(const AColumnTypes: TVSTColumnTypes;
                         const ASheetName: String = 'Лист1';
                         const ADoneMessage: String = 'Выполнено!';
                         const ALandscape: Boolean = False);
var
  Exporter: TSheetsExporter;
  Sheet: TsWorksheet;
  Writer: TSheetWriter;
  VisibleColumnWidths: TIntVector;

  function AlignmentConvert(const AAlignment: TAlignment): TsHorAlignment;
  begin
    Result:= haDefault;
    if AAlignment = taLeftJustify then
      Result:= haLeft
    else if AAlignment = taRightJustify then
      Result:= haRight
    else if AAlignment = taCenter then
      Result:= haCenter;
  end;

  procedure ValueWrite(const ARow, ACol: Integer; const AValue: String; const AType: TVSTColumnType);
  var
    IntValue: Integer;
    DTValue: TDateTime;
    DblValue: Double;
  begin
    if AType = ctInteger then
    begin
      if TryStrToInt(AValue, IntValue) then
        Writer.WriteNumber(ARow, ACol, IntValue, cbtOuter)
      else
        Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
    end
    else if AType = ctDate then
    begin
      if TryStrToDate(AValue, DTValue) then
        Writer.WriteDate(ARow, ACol, DTValue, cbtOuter)
      else
        Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
    end
    else if AType = ctTime then
    begin
      if TryStrToTime(AValue, DTValue) then
        Writer.WriteTime(ARow, ACol, DTValue, cbtOuter)
      else
        Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
    end
    else if AType = ctDouble then
    begin
      if TryStrToFloat(AValue, DblValue) then
        Writer.WriteNumber(ARow, ACol, DblValue, cbtOuter)
      else
        Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
    end
    else Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
  end;

  procedure SheetWrite;
  var
    i, j, R, C: Integer;
  begin

    if HeaderVisible then
    begin
      Writer.SetFont(HeaderFont);
      R:= 1;
      C:= 0;
      for i:= 0 to High(FHeaderCaptions) do
      begin
        if not FColumnVisibles[i] then continue;
        C:= C + 1;
        Writer.SetBackground(FColumnHeaderBGColors[i]);
        Writer.SetAlignment(AlignmentConvert(FTree.Header.Columns[i].CaptionAlignment), vaCenter);
        Writer.WriteText(R, C, FHeaderCaptions[i], cbtOuter);
      end;
    end;

    Writer.SetFont(ValuesFont);
    C:= 0;
    for i:= 0 to High(FDataValues) do
    begin
      if not FColumnVisibles[i] then continue;
      R:= Ord(HeaderVisible);
      C:= C + 1;
      Writer.SetBackground(FColumnValuesBGColors[i]);
      Writer.SetAlignment(AlignmentConvert(FTree.Header.Columns[i].Alignment), vaCenter);
      for j:= 0 to High(FDataValues[i]) do
      begin
        R:= R + 1;
        ValueWrite(R, C, FDataValues[i,j], AColumnTypes[i]);
      end;
    end;
  end;

begin
  Exporter:= TSheetsExporter.Create;
  try
    Sheet:= Exporter.AddWorksheet(ASheetName);

    VisibleColumnWidths:= VCut(FColumnWidths, FColumnVisibles);
    Writer:= TSheetWriter.Create(VisibleColumnWidths, Sheet);
    try
      SheetWrite;
    finally
      FreeAndNil(Writer);
    end;

    if ALandscape then Exporter.PageSettings(spoLandscape);
    Exporter.Save(ADoneMessage);
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TVSTTable.SetAutosizeRowHeights(const AValue: Boolean);
begin
  if FAutosizeRowHeights=AValue then Exit;
  FAutosizeRowHeights:= AValue;

  if FAutosizeRowHeights then
    FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toVariableNodeHeight]
  else
    FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions - [toVariableNodeHeight];

  FTree.ShowHint:= not FAutosizeRowHeights;
  FTree.Refresh;
end;

procedure TVSTTable.SelectNode(Node: PVirtualNode);
var
  NewSelectedIndex: Integer;
begin
  NewSelectedIndex:= -1;
  if Assigned(Node) then
  begin
    NewSelectedIndex:= Node^.Index;
    //cancel same selection
    if NewSelectedIndex=SelectedIndex then Exit;
  end;

  //unselect
  if IsSelected then
    FSelected[SelectedIndex]:= False;
  //select
  if Assigned(Node) then
  begin
    FSelected[NewSelectedIndex]:= True;
    FTree.FocusedNode:= Node;
  end;

  if Assigned(FOnSelect) then FOnSelect;
  FTree.Refresh;
end;

procedure TVSTTable.UnselectNode;
begin
  SelectNode(nil);
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
    if FCanUnselect then
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
  if Key=VK_DELETE then
  begin
    if Assigned(FOnDelKeyDown) then
      FOnDelKeyDown
  end
  else if Key=VK_RETURN then
  begin
    if Assigned(FOnReturnKeyDown) then
      FOnReturnKeyDown
  end
  else if Key=VK_UP then
    MoveSelection(-1)
  else if Key=VK_DOWN then
    MoveSelection(1);
end;

procedure TVSTTable.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if FAutosizeRowHeights then
  begin
    InitialStates:= InitialStates + [ivsMultiline];
    Node^.States:= Node^.States + [vsMultiline];
  end
  else begin
    InitialStates:= InitialStates - [ivsMultiline];
    Node^.States:= Node^.States - [vsMultiline];
  end;
end;

procedure TVSTTable.MeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
var
  Height, i: Integer;
begin
  NodeHeight:= RowHeight;
  if not FAutosizeRowHeights then Exit;

  FTree.Font.Assign(FValuesFont);
  for i:=0 to High(FHeaderCaptions) do
  begin
    Height:= FTree.ComputeNodeHeight(TargetCanvas, Node, i);
    if Height>NodeHeight then
      NodeHeight:= Height;
  end;
end;

procedure TVSTTable.SetCanSelect(const AValue: Boolean);
begin
  if not AValue then UnselectNode;
  inherited SetCanSelect(AValue);
end;

function TVSTTable.GetSelectedIndex: Integer;
begin
  Result:= VIndexOf(FSelected, True);
end;

constructor TVSTTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.Margin:= 0;
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions - [toCheckSupport];
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnKeyDown:= @KeyDown;
  FTree.OnInitNode:= @InitNode;
  FTree.OnMeasureItem:= @MeasureItem;
end;

procedure TVSTTable.ValuesClear;
begin
  UnSelect;
  inherited ValuesClear;
end;

procedure TVSTTable.Draw;
begin
  UnSelect;
  inherited Draw;
end;

procedure TVSTTable.UnSelect;
begin
  if IsSelected then
    UnselectNode;
end;

{ TVSTCheckTable }

function TVSTCheckTable.GetIsAllUnchecked: Boolean;
begin
  Result:= VIsAllFalse(FSelected);
end;

procedure TVSTCheckTable.SetCanSelect(const AValue: Boolean);
begin
  if not AValue then CheckAll(False);
  inherited SetCanSelect(AValue);
end;

function TVSTCheckTable.GetIsAllChecked: Boolean;
begin
  Result:= VIsAllTrue(FSelected);
end;

function TVSTCheckTable.GetChecked(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if not IsIndexCorrect(AIndex) then Exit;
  Result:= FSelected[AIndex];
end;

procedure TVSTCheckTable.SetChecked(const AIndex: Integer; const AValue: Boolean);
begin
  if not IsIndexCorrect(AIndex) then Exit;
  Check(AIndex, AValue);
end;

procedure TVSTCheckTable.CheckNode(Node: PVirtualNode; const AChecked: Boolean);
begin
  if (not Assigned(Node)) or VIsNil(FSelected) then Exit;

  if FMaxCheckedCount>=0 then
    if AChecked and (CheckedCount=FMaxCheckedCount) then
      Exit;

  if AChecked then
    Node^.CheckState:= csCheckedNormal
  else
    Node^.CheckState:= csUnCheckedNormal;
  FSelected[Node^.Index]:= AChecked;

  if Assigned(FOnCheck) then
    FOnCheck(Node^.Index, AChecked);
  if Assigned(FOnSelect) and FCanDoSelectEvent then
    FOnSelect;

  FTree.Refresh;
end;

procedure TVSTCheckTable.CheckAll(const AChecked: Boolean);
var
  Node: PVirtualNode;
begin
  if MIsNil(FDataValues) then Exit;
  if VIsNil(FDataValues[0]) then Exit;

  if StopSelectEventWhileCheckAll then
    FCanDoSelectEvent:= False;

  FTree.Visible:= False;
  try
    Node:= FTree.GetFirst;
    while Assigned(Node) do
    begin
      CheckNode(Node, AChecked);
      Node:= FTree.GetNext(Node);
    end;
  finally
    FTree.Visible:= True;
  end;

  if StopSelectEventWhileCheckAll then
  begin
    FCanDoSelectEvent:= True;
    if Assigned(FOnSelect) then
      FOnSelect;
  end;

  FTree.Refresh;
end;

procedure TVSTCheckTable.Check(const AIndex: Integer; const AChecked: Boolean);
begin
  CheckNode(NodeFromIndex(AIndex), AChecked);
end;

function TVSTCheckTable.GetCheckeds: TBoolVector;
begin
  Result:= VCut(FSelected);
end;

function TVSTCheckTable.GetCheckedCount: Integer;
begin
  Result:= VCountIf(FSelected, True);
end;

function TVSTCheckTable.GetUncheckedCount: Integer;
begin
  Result:= VCountIf(FSelected, False);
end;

procedure TVSTCheckTable.MaxCheckedCountClear;
begin
  FMaxCheckedCount:= -1;
end;

procedure TVSTCheckTable.SetMaxCheckedCount(const AValue: Integer);
var
  i, Delta, N: Integer;
begin
  if FMaxCheckedCount=AValue then Exit;
  FMaxCheckedCount:= AValue;

  if FMaxCheckedCount<0 then Exit;

  i:= CheckedCount;
  if i<=FMaxCheckedCount then Exit;

  Delta:= i - FMaxCheckedCount;
  N:= 0;
  for i:= High(FSelected) downto 0 do
  begin
    if FSelected[i] then
    begin
      Check(i, False);
      N:= N + 1;
    end;
    if N=Delta then break;
  end;
end;

procedure TVSTCheckTable.SetCheckeds(const AValue: TBoolVector);
var
  i: Integer;
begin
  if StopSelectEventWhileCheckAll then
    FCanDoSelectEvent:= False;

  for i:= 0 to High(AValue) do
    Check(i, AValue[i]);

  if StopSelectEventWhileCheckAll then
  begin
    FCanDoSelectEvent:= True;
    if Assigned(FOnSelect) then
      FOnSelect;
  end;

  FTree.Refresh;
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
    if FCanUnselect then
      CheckAll(False)
  end
  else if Button=mbLeft then
    CheckNode(Node, Node^.CheckState=csUncheckedNormal);
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
  Allowed:= False;
end;

constructor TVSTCheckTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toCheckSupport];

  FMaxCheckedCount:= -1;
  FStopSelectEventWhileCheckAll:= True;
  FCanDoSelectEvent:= True;
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnInitNode:= @InitNode;
  FTree.OnChecking:= @Checking;
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

end.

