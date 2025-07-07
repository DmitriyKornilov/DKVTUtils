 unit DK_VSTTables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, VirtualTrees, fpstypes,

  DK_VSTCore, DK_VSTTypes, DK_Vector, DK_Matrix, DK_Const, DK_Color,
  DK_SheetExporter, DK_SheetWriter;

type

  { TVSTSimpleTable }

  TVSTSimpleTable = class(TVSTCustomSimpleTable)
  private
    function GetColCount: Integer;
    function GetRowCount: Integer;
    procedure SetColumnVisibles(AValue: TBoolVector);
  protected
    FSelected: TBoolVector;
    FColumnVisibles: TBoolVector;

    function GetIsSelected: Boolean;
    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;

    procedure HeaderClear; override;

    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);
    function IsIndexCorrect(const AIndex: Integer): Boolean;
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    procedure ValuesClear; override;
    procedure Draw; override;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter); override;
    procedure SetColumn(const AColIndex: Integer; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetColumn(const ACaption: String; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);

    procedure Show(const AIndex: Integer);

    property IsSelected: Boolean read GetIsSelected;

    property RowCount: Integer read GetRowCount;
    property ColCount: Integer read GetColCount;

    property ColumnVisibles: TBoolVector read FColumnVisibles write SetColumnVisibles;
  end;

  { TVSTTable }

  TVSTTable = class(TVSTSimpleTable)
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

    procedure SetCanSelect(AValue: Boolean); override;
    function GetSelectedIndex: Integer;

    procedure MoveSelection(const ADeltaIndex: Integer);

    procedure SetAutosizeRowHeights(AValue: Boolean);
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

  TVSTCheckTable = class(TVSTSimpleTable)
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
    procedure Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var {%H-}Allowed: Boolean);

    function GetIsAllChecked: Boolean;
    function GetIsAllUnchecked: Boolean;
    procedure SetMaxCheckedCount(AValue: Integer);

    procedure SetCanSelect(AValue: Boolean); override;

    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    function GetChecked(AIndex: Integer): Boolean;
    function GetSelected: TBoolVector;
    procedure SetSelected(AValue: TBoolVector);

    function GetCheckedCount: Integer;
    function GetUncheckedCount: Integer;

    procedure CheckNode(Node: PVirtualNode; const AChecked: Boolean);
    procedure Check(Node: PVirtualNode);
    procedure Check(const AIndex: Integer);
    procedure Uncheck(Node: PVirtualNode);
    procedure Uncheck(const AIndex: Integer);
    procedure ReverseCheckState(Node: PVirtualNode);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    procedure ValuesClear; override;
    procedure Draw; override;

    procedure CheckAll(const AChecked: Boolean);

    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
    property IsAllChecked: Boolean read GetIsAllChecked;
    property IsAllUnchecked: Boolean read GetIsAllUnchecked;
    property Selected: TBoolVector read GetSelected write SetSelected;

    property CheckedCount: Integer read GetCheckedCount;
    property UncheckedCount: Integer read GetUncheckedCount;
    property MaxCheckedCount: Integer read FMaxCheckedCount write SetMaxCheckedCount;
    procedure MaxCheckedCountClear;

    property StopSelectEventWhileCheckAll: Boolean read FStopSelectEventWhileCheckAll write FStopSelectEventWhileCheckAll;
    property OnCheck: TVSTRowCheckEvent read FOnCheck write FOnCheck;
  end;

  { TVSTCoreCategoryTable }

  TVSTCoreCategoryTable = class(TVSTCoreTable)
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
    FCheckEnable: Boolean;
    FCheckVisible: Boolean;

    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var {%H-}Allowed: Boolean);

    procedure SetCanSelect(AValue: Boolean); override;
    procedure SetCheckEnable(AValue: Boolean);
    procedure SetCheckVisible(AValue: Boolean);

    procedure SetSelected(AValue: TBoolMatrix);
    function GetSelected: TBoolMatrix;

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
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);

    function IsCategoryAllChecked(const AIndex: Integer): Boolean;
    function IsCategoryAllUnchecked(const AIndex: Integer): Boolean;

    procedure CheckAll(const AChecked: Boolean);
    procedure CheckCategory(const AIndex: Integer; const AChecked: Boolean);

    property Checked[AIndex1, AIndex2: Integer]: Boolean read GetChecked write SetChecked;
    property Selected: TBoolMatrix read GetSelected write SetSelected;

    property CheckEnable: Boolean read FCheckEnable write SetCheckEnable;
    property CheckVisible: Boolean read FCheckVisible write SetCheckVisible;

    property StopSelectEventWhileCheckAll: Boolean read FStopSelectEventWhileCheckAll write FStopSelectEventWhileCheckAll;
    property OnCheck: TVSTRowCheckEvent read FOnCheck write FOnCheck;
  end;

implementation

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

procedure TVSTCategoryCheckTable.SetCheckEnable(AValue: Boolean);
begin
  if FCheckEnable=AValue then Exit;
  FCheckEnable:= AValue;
  CanSelect:= CheckEnable;
  FTree.ReinitNode(FTree.RootNode, True);
  FTree.Refresh;
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
  for i:= 0 to High(AValue) do
  begin
    for j:= 0 to High(AValue[i]) do
    begin
      if AValue[i, j] then
        Check(i, j)
      else
        Uncheck(i, j);
    end;
  end;
end;

procedure TVSTCategoryCheckTable.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if CheckEnable and CheckVisible then
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
  end
  else Node^.CheckType:= ctNone;
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
    if FCanUnselect then
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

  if Assigned(FOnCheck) then
    FOnCheck(Node^.Index, AChecked);
  if Assigned(FOnSelect) and FCanDoSelectEvent then
    FOnSelect;

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

constructor TVSTCategoryCheckTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);

  FStopSelectEventWhileCheckAll:= False;
  FCanDoSelectEvent:= True;
  FCheckEnable:= True;
  FCheckVisible:= True;
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnInitNode:= @InitNode;
  FTree.OnChecking:= @Checking;
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

function TVSTCategoryCheckTable.IsAllCheckedCategory(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsCategoryAllChecked(Node^.Index);
end;

function TVSTCategoryCheckTable.IsAllUncheckedCategory(Node: PVirtualNode): Boolean;
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

  if StopSelectEventWhileCheckAll then
    FCanDoSelectEvent:= False;

  FTree.Visible:= False;
  try
    Node:= FTree.GetFirst;
    while Assigned(Node) do
    begin
      if AChecked then
        Check(Node)
      else
        Uncheck(Node);
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

procedure TVSTCheckTable.SetMaxCheckedCount(AValue: Integer);
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
      Uncheck(i);
      N:= N + 1;
    end;
    if N=Delta then break;
  end;
end;

procedure TVSTCheckTable.SetSelected(AValue: TBoolVector);
var
  i: Integer;
begin
  for i:= 0 to High(AValue) do
  begin
    if AValue[i] then
      Check(i)
    else
      Uncheck(i);
  end;
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

constructor TVSTCheckTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toCheckSupport];

  FMaxCheckedCount:= -1;
  FStopSelectEventWhileCheckAll:= False;
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

{ TVSTSimpleTable }

procedure TVSTSimpleTable.SetColumnVisibles(AValue: TBoolVector);
var
  i: Integer;
begin
  if Length(AValue)<>Length(FHeaderCaptions) then Exit;
  FColumnVisibles:= VCut(AValue);
  for i:= 0 to High(FColumnVisibles) do
  begin
    if FColumnVisibles[i] then
      FTree.Header.Columns[i].Options:= FTree.Header.Columns[i].Options + [coVisible]
    else
      FTree.Header.Columns[i].Options:= FTree.Header.Columns[i].Options - [coVisible];
  end;
  FTree.Refresh;
end;

function TVSTSimpleTable.GetColCount: Integer;
begin
  Result:= Length(FHeaderCaptions);
end;

function TVSTSimpleTable.GetRowCount: Integer;
begin
  Result:= 0;
  if not MIsNil(FDataValues) then
    Result:= Length(FDataValues[0]);
end;

function TVSTSimpleTable.GetIsSelected: Boolean;
begin
  Result:= False;
  if not Assigned(Self) then Exit;
  Result:= VIsTrue(FSelected);
end;

function TVSTSimpleTable.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  Result:= FSelected[Node^.Index];
end;

procedure TVSTSimpleTable.HeaderClear;
begin
  inherited HeaderClear;
  FDataValues:= nil;
  FColumnVisibles:= nil;
end;

procedure TVSTSimpleTable.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
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

function TVSTSimpleTable.IsIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FDataValues) then Exit;
  if VIsNil(FDataValues[0]) then Exit;
  Result:= (AIndex>=0) and (AIndex<=High(FDataValues[0]));
end;

constructor TVSTSimpleTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
  FTree.OnGetText:= @GetText;
end;

procedure TVSTSimpleTable.ValuesClear;
begin
  FSelected:= nil;
  inherited ValuesClear;
end;

procedure TVSTSimpleTable.Draw;
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

  inherited Draw;
end;

procedure TVSTSimpleTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
begin
  inherited AddColumn(ACaption, AWidth, ACaptionAlignment);
  MAppend(FDataValues, nil);
  VAppend(FColumnVisibles, True);
end;

procedure TVSTSimpleTable.SetColumn(const AColIndex: Integer;
  const AValues: TStrVector; const AValuesAlignment: TAlignment);
begin
  FDataValues[AColIndex]:= VCut(AValues);
  FTree.Header.Columns[AColIndex].Alignment:= AValuesAlignment;
end;

procedure TVSTSimpleTable.SetColumn(const ACaption: String;
  const AValues: TStrVector; const AValuesAlignment: TAlignment);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColIndex>=0 then
    SetColumn(ColIndex, AValues, AValuesAlignment);
end;

procedure TVSTSimpleTable.Show(const AIndex: Integer);
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
    Writer:= TSheetWriter.Create(VisibleColumnWidths, Sheet, nil, RowHeight);
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

procedure TVSTTable.SetAutosizeRowHeights(AValue: Boolean);
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

procedure TVSTTable.SetCanSelect(AValue: Boolean);
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

end.

