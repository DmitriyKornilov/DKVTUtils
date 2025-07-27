unit DK_VSTCategoryTables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, VirtualTrees,

  DK_VSTCore, DK_VSTTypes, DK_Vector, DK_Matrix, DK_Const, DK_Color;

type
  TVSTCategoryCheckKind = (chkNone, chkCategory, chkValues, chkAll);

  { TVSTCoreCategoryTable }

  TVSTCoreCategoryTable = class(TVSTCore)
  protected
    FDataValues: TStrMatrix3D;
    FSelected: TBoolMatrix;
    FCategoryFont: TFont;

    procedure SetTreeLinesVisible(AValue: Boolean);
    procedure HeaderClear; override;
    function IsIndexesCorrect(const AIndex1, AIndex2: Integer): Boolean;

    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    function GetIsSelected: Boolean;
    procedure SetCategoryFont(AValue: TFont);
    procedure CellFont(Node: PVirtualNode; {%H-}Column: TColumnIndex); override;
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor Destroy; override;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter); override;
    procedure SetColumn(const AColIndex: Integer; const AValues: TStrMatrix;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetColumn(const ACaption: String; const AValues: TStrMatrix;
                        const AValuesAlignment: TAlignment = taCenter);

    procedure ValuesClear; override;

    procedure ExpandAll(const AExpand: Boolean);
    procedure Show(const AIndex1, AIndex2: Integer);
    procedure ShowFirst;

    procedure SetSingleFont(const AFont: TFont); override;
    property CategoryFont: TFont read FCategoryFont write SetCategoryFont;

    property TreeLinesVisible: Boolean write SetTreeLinesVisible;
    property IsSelected: Boolean read GetIsSelected;
  end;

  { TVSTCustomCategoryTable }

  TVSTCustomCategoryTable = class(TVSTCoreCategoryTable)
  protected
    FVCategoryValues: TStrVector;
    FMCategoryValues: TStrMatrix;
    FOneCategoryColumn: Boolean;

    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);

    procedure SetCategories(const AValues: TStrVector);
    procedure SetCategories(const AValues: TStrMatrix);

    procedure ValuesClear; override;
    procedure Draw;
  end;

  { TVSTCategoryRadioTable }

  TVSTCategoryRadioTable = class(TVSTCustomCategoryTable)
  protected
    FRadioEnable: Boolean;
    FRadioVisible: Boolean;

    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);

    procedure SetCanSelect(AValue: Boolean); override;
    procedure SetRadioEnable(AValue: Boolean);
    procedure SetRadioVisible(AValue: Boolean);

    procedure SelectNode(Node: PVirtualNode);
    procedure UnselectNode;

    function GetSelectedIndex1: Integer;
    function GetSelectedIndex2: Integer;
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    procedure Draw;

    procedure ValuesClear; override;

    procedure Select(const AIndex1, AIndex2: Integer);
    procedure SelectedIndexes(out AIndex1, AIndex2: Integer);
    property SelectedIndex1: Integer read GetSelectedIndex1;
    property SelectedIndex2: Integer read GetSelectedIndex2;

    property RadioEnable: Boolean read FRadioEnable write SetRadioEnable;
    property RadioVisible: Boolean read FRadioVisible write SetRadioVisible;
  end;

  { TVSTCategoryCheckTable }

  TVSTCategoryCheckTable = class(TVSTCustomCategoryTable)
  protected
    FOnCheck: TVSTRowCheckEvent;
    //FStopSelectEventWhileCheckAll=True - OnSelect вызывается только после заверешения CheckAll
    //FStopSelectEventWhileCheckAll=False - OnSelect вызывается на изменение каждой позиции (default)
    FStopSelectEventWhileCheckAll: Boolean;
    FCanDoSelectEvent: Boolean;

    FCheckVisible: Boolean;
    FCheckKind: TVSTCategoryCheckKind;

    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure Checking(Sender: TBaseVirtualTree; {%H-}Node: PVirtualNode;
      var {%H-}NewState: TCheckState; var Allowed: Boolean);

    procedure SetCanSelect(AValue: Boolean); override;
    procedure SetCheckVisible(AValue: Boolean);
    procedure SetCheckKind(AValue: TVSTCategoryCheckKind);

    procedure SetSelected(AValue: TBoolMatrix);
    function GetSelected: TBoolMatrix;

    function GetCategorySelected: TBoolVector;
    procedure SetCategorySelected(AValue: TBoolVector);

    procedure SetChecked(AIndex1, AIndex2: Integer; AValue: Boolean);
    function GetChecked(AIndex1, AIndex2: Integer): Boolean;

    procedure CheckNode(Node: PVirtualNode; const AChecked: Boolean);
    procedure Check(const AIndex1, AIndex2: Integer; const AChecked: Boolean);
    procedure CheckCategory(Node: PVirtualNode; const AChecked: Boolean);

    function IsCategoryAllChecked(Node: PVirtualNode): Boolean;
    function IsCategoryAllUnchecked(Node: PVirtualNode): Boolean;
    function IsCategoryHasChecked(Node: PVirtualNode): Boolean;
    function IsCategoryHasChecked(const AIndex: Integer): Boolean;

    procedure SetCategoryCheckState(Node: PVirtualNode);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);

    function IsCategoryAllChecked(const AIndex: Integer): Boolean;
    function IsCategoryAllUnchecked(const AIndex: Integer): Boolean;

    procedure CheckAll(const AChecked: Boolean);
    procedure CheckCategory(const AIndex: Integer; const AChecked: Boolean);

    property Checked[AIndex1, AIndex2: Integer]: Boolean read GetChecked write SetChecked;
    property Selected: TBoolMatrix read GetSelected write SetSelected;
    property CategorySelected: TBoolVector read GetCategorySelected write SetCategorySelected;

    property CheckVisible: Boolean read FCheckVisible write SetCheckVisible;
    property CheckKind: TVSTCategoryCheckKind read FCheckKind write SetCheckKind;
    property OnCheck: TVSTRowCheckEvent read FOnCheck write FOnCheck;

    property StopSelectEventWhileCheckAll: Boolean read FStopSelectEventWhileCheckAll write FStopSelectEventWhileCheckAll;
  end;

implementation

{ TVSTCoreCategoryTable }

procedure TVSTCoreCategoryTable.SetCategoryFont(AValue: TFont);
begin
  FCategoryFont.Assign(AValue);
  FTree.Refresh;
end;

procedure TVSTCoreCategoryTable.SetTreeLinesVisible(AValue: Boolean);
begin
  if Avalue then
    FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions + [toShowTreeLines]
  else
    FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
end;

procedure TVSTCoreCategoryTable.HeaderClear;
begin
  inherited HeaderClear;
  FDataValues:= nil;
end;

function TVSTCoreCategoryTable.IsIndexesCorrect(const AIndex1, AIndex2: Integer): Boolean;
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

function TVSTCoreCategoryTable.IsCellSelected(Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  Result:= False;
  if FTree.GetNodeLevel(Node)<>1 then Exit;
  Result:= FSelected[(Node^.Parent)^.Index, Node^.Index];
end;

function TVSTCoreCategoryTable.GetIsSelected: Boolean;
var
  Ind1, Ind2: Integer;
begin
  Result:= False;
  if not Assigned(Self) then Exit;
  MIndexOf(FSelected, True, Ind1, Ind2);
  Result:= (Ind1>=0) and (Ind2>=0);
end;

procedure TVSTCoreCategoryTable.CellFont(Node: PVirtualNode; Column: TColumnIndex);
begin
  if FTree.GetNodeLevel(Node)=0 then
    FCellFont.Assign(FCategoryFont)
  else
    inherited CellFont(Node, Column);
end;

constructor TVSTCoreCategoryTable.Create(const ATree: TVirtualStringTree;
  const AHeaderHeight: Integer; const ARowHeight: Integer);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);

  FCategoryFont:= TFont.Create;
  FCategoryFont.Assign(FTree.Font);

  FTree.Margin:= 0;
  FCanSelect:= True;
  FSelectedBGColor:= FTree.Color;
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toCheckSupport];
  FTree.LineStyle:= lsSolid;
end;

destructor TVSTCoreCategoryTable.Destroy;
begin
  FreeAndNil(FCategoryFont);
  inherited Destroy;
end;

procedure TVSTCoreCategoryTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
begin
  inherited AddColumn(ACaption, AWidth, ACaptionAlignment);
  MAppend(FDataValues, nil);
end;

procedure TVSTCoreCategoryTable.SetColumn(const AColIndex: Integer;
  const AValues: TStrMatrix; const AValuesAlignment: TAlignment);
begin
  FDataValues[AColIndex]:= MCut(AValues);
  FTree.Header.Columns[AColIndex].Alignment:= AValuesAlignment;
end;

procedure TVSTCoreCategoryTable.SetColumn(const ACaption: String;
  const AValues: TStrMatrix; const AValuesAlignment: TAlignment);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColIndex>=0 then
    SetColumn(ColIndex, AValues, AValuesAlignment);
end;

procedure TVSTCoreCategoryTable.ValuesClear;
var
  i: Integer;
begin
  FSelected:= nil;
  for i:=0 to High(FDataValues) do
    FDataValues[i]:= nil;
  inherited ValuesClear;
end;

procedure TVSTCoreCategoryTable.ExpandAll(const AExpand: Boolean);
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

procedure TVSTCoreCategoryTable.Show(const AIndex1, AIndex2: Integer);
var
  Node: PVirtualNode;
begin
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  Node:= NodeFromIndex(AIndex1, AIndex2);
  if not Assigned(Node) then Exit;
  FTree.Expanded[Node^.Parent]:= True;
  FTree.FocusedNode:= Node;
end;

procedure TVSTCoreCategoryTable.ShowFirst;
var
  Node: PVirtualNode;
begin
  if not IsIndexesCorrect(0, 0) then Exit;
  Node:= NodeFromIndex(0, 0)^.Parent;
  if not Assigned(Node) then Exit;
  FTree.FocusedNode:= Node;
end;

procedure TVSTCoreCategoryTable.SetSingleFont(const AFont: TFont);
begin
  inherited SetSingleFont(AFont);
  CategoryFont:= AFont;
end;

{ TVSTCustomCategoryTable }

procedure TVSTCustomCategoryTable.GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  i, j, k: Integer;
begin
  CellText:= EmptyStr;
  if FTree.GetNodeLevel(Node)=0 then
  begin
    i:= Node^.Index;
    if FOneCategoryColumn then
    begin
      if Column=0 then
        CellText:= FVCategoryValues[i];
    end
    else begin
      if Column>=0 then
        CellText:= FMCategoryValues[i, Column];
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

constructor TVSTCustomCategoryTable.Create(const ATree: TVirtualStringTree;
  const AHeaderHeight: Integer; const ARowHeight: Integer);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FOneCategoryColumn:= True;
  FTree.OnGetText:= @GetText;
end;

procedure TVSTCustomCategoryTable.SetCategories(const AValues: TStrVector);
begin
  FVCategoryValues:= VCut(AValues);
  FOneCategoryColumn:= True;
end;

procedure TVSTCustomCategoryTable.SetCategories(const AValues: TStrMatrix);
begin
  FMCategoryValues:= MCut(AValues);
  FOneCategoryColumn:= False;
end;

procedure TVSTCustomCategoryTable.ValuesClear;
begin
  FVCategoryValues:= nil;
  FMCategoryValues:= nil;
  inherited ValuesClear;
end;

procedure TVSTCustomCategoryTable.Draw;
var
  ColIndex, CategoryIndex, CategoryCount, MaxLength: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;
  if MIsNil(FDataValues) then Exit;

  CategoryCount:= 0;
  for ColIndex:= 0 to High(FHeaderCaptions) do
  begin
    if Length(FDataValues[ColIndex])>CategoryCount then
      CategoryCount:= Length(FDataValues[ColIndex]);
  end;

  for ColIndex:= 0 to High(FHeaderCaptions) do
    MReDim(FDataValues[ColIndex], CategoryCount);

  if FOneCategoryColumn then
    VReDim(FVCategoryValues, CategoryCount, EmptyStr)
  else
    MReDim(FMCategoryValues, CategoryCount, Length(FHeaderCaptions), EmptyStr);

  for CategoryIndex:= 0 to CategoryCount-1 do
  begin
    MaxLength:= 0;
    for ColIndex:= 0 to High(FHeaderCaptions) do
    begin
      if Length(FDataValues[ColIndex, CategoryIndex])>MaxLength then
        MaxLength:= Length(FDataValues[ColIndex, CategoryIndex]);
    end;
    for ColIndex:= 0 to High(FHeaderCaptions) do
      VReDim(FDataValues[ColIndex, CategoryIndex], MaxLength);
  end;

  MReDim(FSelected, CategoryCount);
  for CategoryIndex:= 0 to CategoryCount-1 do
    VReDim(FSelected[CategoryIndex], Length(FDataValues[0,CategoryIndex]));

  VSTLoad(FTree, FDataValues[0], False);

  SetColumnWidths;
end;

{ TVSTCategoryRadioTable }

function TVSTCategoryRadioTable.GetSelectedIndex1: Integer;
var
  Ind1, Ind2: Integer;
begin
  SelectedIndexes(Ind1, Ind2);
  Result:= Ind1;
end;

function TVSTCategoryRadioTable.GetSelectedIndex2: Integer;
var
  Ind1, Ind2: Integer;
begin
  SelectedIndexes(Ind1, Ind2);
  Result:= Ind2;
end;

procedure TVSTCategoryRadioTable.InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if RadioEnable and RadioVisible then
  begin
    if FTree.GetNodeLevel(Node)=1 then
    begin
      Node^.CheckType:= ctRadioButton;
      Node^.CheckState:= csUncheckedNormal;
    end;
  end
  else Node^.CheckType:= ctNone;
end;

procedure TVSTCategoryRadioTable.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  if not FCanSelect then Exit;

  Node:= FTree.GetNodeAt(X, Y);
  if not Assigned(Node) then Exit;

  if Button=mbRight then
  begin
    if FCanUnselect then
      UnselectNode;
  end
  else if Button=mbLeft then
  begin
    if FTree.GetNodeLevel(Node)=1 then
      SelectNode(Node);
  end;
end;

procedure TVSTCategoryRadioTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then UnselectNode;
  inherited SetCanSelect(AValue);
end;

procedure TVSTCategoryRadioTable.SetRadioEnable(AValue: Boolean);
begin
  if FRadioEnable=AValue then Exit;
  FRadioEnable:= AValue;
  CanSelect:= RadioEnable;
  FTree.ReinitNode(FTree.RootNode, True);
  FTree.Refresh;
end;

procedure TVSTCategoryRadioTable.SetRadioVisible(AValue: Boolean);
begin
  if FRadioVisible=AValue then Exit;
  FRadioVisible:= AValue;
  if FRadioVisible then
    SelectedBGColor:= FTree.Color
  else
    SelectedBGColor:= DefaultSelectionBGColor;
  FTree.ReinitNode(FTree.RootNode, True);
  FTree.Refresh;
end;

procedure TVSTCategoryRadioTable.SelectNode(Node: PVirtualNode);
var
  SelectedNode: PVirtualNode;
  i, j: Integer;
begin
  //unselect
  if IsSelected then
  begin
    SelectedIndexes(i,j);
    SelectedNode:= NodeFromIndex(i,j);
    SelectedNode^.CheckState:= csUncheckedNormal;
    FSelected[i,j]:= False;
  end;
  //select
  if Assigned(Node) then
  begin
    i:= (Node^.Parent)^.Index;
    j:= Node^.Index;
    FSelected[i, j]:= True;
    FTree.FocusedNode:= Node;
    Node^.CheckState:= csCheckedNormal;
  end;

  if Assigned(FOnSelect) then FOnSelect;

  FTree.Refresh;
end;

procedure TVSTCategoryRadioTable.UnselectNode;
begin
  SelectNode(nil);
end;

constructor TVSTCategoryRadioTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FRadioEnable:= True;
  FRadioVisible:= True;
  FTree.OnInitNode:= @InitNode;
  FTree.OnMouseDown:= @MouseDown;
end;

procedure TVSTCategoryRadioTable.Draw;
begin
  UnselectNode;
  inherited Draw;
  FTree.Refresh;
end;

procedure TVSTCategoryRadioTable.ValuesClear;
begin
  UnselectNode;
  inherited ValuesClear;
end;

procedure TVSTCategoryRadioTable.Select(const AIndex1, AIndex2: Integer);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(AIndex1, AIndex2);
  if Assigned(Node) then SelectNode(Node);
end;

procedure TVSTCategoryRadioTable.SelectedIndexes(out AIndex1, AIndex2: Integer);
begin
  MIndexOf(FSelected, True, AIndex1, AIndex2);
end;

{ TVSTCategoryCheckTable }

function TVSTCategoryCheckTable.GetChecked(AIndex1, AIndex2: Integer): Boolean;
begin
  Result:= False;
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  Result:= FSelected[AIndex1, AIndex2];
end;

function TVSTCategoryCheckTable.GetSelected: TBoolMatrix;
begin
  Result:= MCut(FSelected);
end;

procedure TVSTCategoryCheckTable.SetCheckVisible(AValue: Boolean);
begin
  if FCheckVisible=AValue then Exit;
  FCheckVisible:= AValue;
  if FCheckVisible then
    SelectedBGColor:= FTree.Color
  else
    SelectedBGColor:= DefaultSelectionBGColor;
  FTree.ReinitNode(FTree.RootNode, True);
  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.SetSelected(AValue: TBoolMatrix);
var
  i, j: Integer;
begin
  if StopSelectEventWhileCheckAll then
    FCanDoSelectEvent:= False;

  for i:= 0 to High(AValue) do
    for j:= 0 to High(AValue[i]) do
      Check(i, j, AValue[i, j]);

  if StopSelectEventWhileCheckAll then
  begin
    FCanDoSelectEvent:= True;
    if Assigned(FOnSelect) then
      FOnSelect;
  end;

  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.SetCheckKind(AValue: TVSTCategoryCheckKind);
begin
  if FCheckKind=AValue then Exit;
  FCheckKind:=AValue;
  CheckAll(False);
  CanSelect:= FCheckKind<>chkNone;
  FTree.ReinitNode(FTree.RootNode, True);
  FTree.Refresh;
end;

function TVSTCategoryCheckTable.GetCategorySelected: TBoolVector;
var
  i: Integer;
begin
  VDim(Result{%H-}, Length(FSelected));
  for i:= 0 to High(FSelected) do
    Result[i]:= IsCategoryAllChecked(i);
end;

procedure TVSTCategoryCheckTable.SetCategorySelected(AValue: TBoolVector);
var
  i: Integer;
begin
  if StopSelectEventWhileCheckAll then
    FCanDoSelectEvent:= False;

  for i:= 0 to High(AValue) do
      CheckCategory(i, AValue[i]);

  if StopSelectEventWhileCheckAll then
  begin
    FCanDoSelectEvent:= True;
    if Assigned(FOnSelect) then
      FOnSelect;
  end;

  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if (CheckKind=chkNone) or (not CheckVisible) then
  begin
    Node^.CheckType:= ctNone;
    Exit;
  end;

  if FTree.GetNodeLevel(Node)=0 then
  begin
    if CheckKind=chkValues then
      Node^.CheckType:= ctNone
    else begin
      if CheckKind=chkAll then
        Node^.CheckType:= ctTriStateCheckBox
      else  //chkCategory
        Node^.CheckType:= ctCheckBox;
      Node^.CheckState:= csUncheckedNormal;
    end;
  end
  else if FTree.GetNodeLevel(Node)=1 then
  begin
     if CheckKind=chkCategory then
      Node^.CheckType:= ctNone
    else begin  // chkValues, chkAll
      Node^.CheckType:= ctCheckBox;
      Node^.CheckState:= csUncheckedNormal;
    end;
  end;
end;

procedure TVSTCategoryCheckTable.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  if CheckKind=chkNone then Exit;

  Node:= FTree.GetNodeAt(X, Y);
  if not Assigned(Node) then Exit;

  if Button=mbRight then
  begin
    if FCanUnselect then
      CheckAll(False);
  end
  else if Button=mbLeft then
  begin
    if FTree.GetNodeLevel(Node)=0 then
    begin
      if CheckKind in [chkCategory, chkAll] then
        CheckCategory(Node, Node^.CheckState=csUncheckedNormal)
    end
    else if FTree.GetNodeLevel(Node)=1 then
    begin
      if CheckKind=chkCategory then
        CheckCategory(Node^.Parent, (Node^.Parent)^.CheckState=csUncheckedNormal)
      else begin // chkValues, chkAll
        CheckNode(Node, Node^.CheckState=csUncheckedNormal);
        if CheckKind=chkAll then
          SetCategoryCheckState(Node^.Parent);
      end;
    end;
  end;

  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.Checking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed:= False;
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
  begin
    if CheckKind in [chkValues, chkAll] then
      Node^.CheckState:= csCheckedNormal
  end
  else begin
    if CheckKind in [chkValues, chkAll] then
      Node^.CheckState:= csUnCheckedNormal;
  end;
  FSelected[(Node^.Parent)^.Index, Node^.Index]:= AChecked;

  if Assigned(FOnCheck) then
    FOnCheck(Node^.Index, AChecked);
  if Assigned(FOnSelect) and FCanDoSelectEvent then
    FOnSelect;

  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.Check(const AIndex1, AIndex2: Integer;
  const AChecked: Boolean);
begin
  CheckNode(NodeFromIndex(AIndex1, AIndex2), AChecked);
end;

procedure TVSTCategoryCheckTable.SetChecked(AIndex1, AIndex2: Integer;
  AValue: Boolean);
begin
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  Check(AIndex1, AIndex2, AValue);
end;

constructor TVSTCategoryCheckTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);

  FStopSelectEventWhileCheckAll:= True;
  FCanDoSelectEvent:= True;
  FCheckKind:= chkAll;
  FCheckVisible:= True;

  FTree.OnMouseDown:= @MouseDown;
  FTree.OnInitNode:= @InitNode;
  FTree.OnChecking:= @Checking;
end;

function TVSTCategoryCheckTable.IsCategoryHasChecked(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsCategoryHasChecked(Node^.Index);
end;

function TVSTCategoryCheckTable.IsCategoryHasChecked(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[AIndex]) then Exit;
  Result:= VIsTrue(FSelected[AIndex]);
end;

function TVSTCategoryCheckTable.IsCategoryAllChecked(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[AIndex]) then Exit;
  Result:= VIsAllTrue(FSelected[AIndex]);
end;

function TVSTCategoryCheckTable.IsCategoryAllUnchecked(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[AIndex]) then Exit;
  Result:= VIsAllFalse(FSelected[AIndex]);
end;

function TVSTCategoryCheckTable.IsCategoryAllChecked(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsCategoryAllChecked(Node^.Index);
end;

function TVSTCategoryCheckTable.IsCategoryAllUnchecked(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsCategoryAllUnchecked(Node^.Index);
end;

procedure TVSTCategoryCheckTable.CheckAll(const AChecked: Boolean);
var
  Node: PVirtualNode;
begin
  if StopSelectEventWhileCheckAll then
    FCanDoSelectEvent:= False;

  FTree.Visible:= False;
  try
    Node:= FTree.GetFirst;
    while Assigned(Node) do
    begin
      if FTree.GetNodeLevel(Node)=0 then
        CheckCategory(Node, AChecked);
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
    Check(Ind, i, AChecked);
    if AChecked then
    begin
      if CheckKind in [chkCategory, chkAll] then
        Node^.CheckState:= csCheckedNormal
    end
    else begin
      if CheckKind in [chkCategory, chkAll] then
        Node^.CheckState:= csUnCheckedNormal;
    end;
  end;
end;

procedure TVSTCategoryCheckTable.SetCategoryCheckState(Node: PVirtualNode);
begin
  if IsCategoryAllChecked(Node) then
   Node^.CheckState:= csCheckedNormal
  else if IsCategoryAllUnchecked(Node) then
    Node^.CheckState:= csUncheckedNormal
  else if IsCategoryHasChecked(Node) then
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

end.

