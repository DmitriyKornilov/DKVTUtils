unit DK_VSTTables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, VirtualTrees,
  DK_VSTUtils, DK_Vector, DK_Matrix;

type

  { TVSTCoreTable }

  TVSTCoreTable = class(TObject)
  private
    procedure SetDrawBorders(AValue: Boolean);
  protected
    FTree: TVirtualStringTree;

    FBorderColor: TColor;
    FDrawBorders: Boolean;

    FCanSelect: Boolean;
    FSelectedNode: PVirtualNode;

    FValuesBGColor: TColor;
    FHeaderBGColor: TColor;
    FSelectedBGColor: TColor;

    FHeaderCaptions: TStrVector;
    FColumnWidths: TIntVector;

    FHeaderFont: TFont;
    FValuesFont: TFont;
    FSelectedFont: TFont;

    procedure SetCanSelect(AValue: Boolean); virtual;
    procedure HeaderClear; virtual;
    procedure SetHeaderVisible(AValue: Boolean);
    procedure SetColumnWidths;
    procedure SetDrawBorders;
    procedure SetBorderColor(AValue: TColor);
    procedure SetHeaderBGColor(AValue: TColor);
    procedure SetHeaderFont(AValue: TFont);
    procedure SetSelectedBGColor(AValue: TColor);
    procedure SetSelectedFont(AValue: TFont);
    procedure SetValuesBGColor(AValue: TColor);
    procedure SetValuesFont(AValue: TFont);
    procedure HeaderDrawQueryElements(Sender: TVTHeader;
                            var PaintInfo: THeaderPaintInfo;
                            var Elements: THeaderPaintElements);
    procedure AdvancedHeaderDraw(Sender: TVTHeader;
                            var PaintInfo: THeaderPaintInfo;
                            const Elements: THeaderPaintElements);


  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    property DrawBorders: Boolean read FDrawBorders write SetDrawBorders;

    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property ValuesBGColor: TColor read FValuesBGColor write SetValuesBGColor;
    property HeaderBGColor: TColor read FHeaderBGColor write SetHeaderBGColor;
    property SelectedBGColor: TColor read FSelectedBGColor write SetSelectedBGColor;

    property CanSelect: Boolean read FCanSelect write SetCanSelect;

    property HeaderVisible: Boolean write SetHeaderVisible;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property ValuesFont: TFont read FValuesFont write SetValuesFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
  end;

  { TVSTCustomTable }

  TVSTCustomTable = class(TVSTCoreTable)
  protected
    FDataValues: TStrMatrix;

    procedure HeaderClear; override;

    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; TextType: TVSTTextType;
                      var CellText: String);

    function IsIndexCorrect(const AIndex: Integer): Boolean;
    function NodeFromIndex(const AIndex: Integer): PVirtualNode;
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;
    procedure Clear;
    procedure ValuesClear; virtual;
    procedure Draw; virtual;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter);
    procedure SetColumn(const AColumnIndex: Integer; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetColumn(const ACaption: String; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);

  end;


  { TVSTTable }

  TVSTTable = class(TVSTCustomTable)
  protected
    FSelectedIndex: Integer;

    procedure Select(Node: PVirtualNode);
    procedure UnselectNode;
    function GetIsSelected: Boolean;

    procedure DrawText(Sender: TBaseVirtualTree;
                       TargetCanvas: TCanvas; Node: PVirtualNode;
                       Column: TColumnIndex;
                       const CellText: String; const CellRect: TRect;
                       var DefaultDraw: Boolean);
    procedure BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                              Node: PVirtualNode; Column: TColumnIndex;
                              CellPaintMode: TVTCellPaintMode; CellRect: TRect;
                              var ContentRect: TRect);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        Shift: TShiftState; X, Y: Integer);

    procedure SetCanSelect(AValue: Boolean); override;

  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure ValuesClear; override;

    procedure Draw; override;

    procedure Select(const AIndex: Integer);
    property SelectedIndex: Integer read FSelectedIndex;
    property IsSelected: Boolean read GetIsSelected;
  end;

  { TVSTCheckTable }

  TVSTCheckTable = class(TVSTCustomTable)
  private
    FChecked: TBoolVector;


    procedure DrawText(Sender: TBaseVirtualTree;
                       TargetCanvas: TCanvas; Node: PVirtualNode;
                       Column: TColumnIndex;
                       const CellText: String; const CellRect: TRect;
                       var DefaultDraw: Boolean);
    procedure BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                              Node: PVirtualNode; Column: TColumnIndex;
                              CellPaintMode: TVTCellPaintMode; CellRect: TRect;
                              var ContentRect: TRect);


    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        Shift: TShiftState; X, Y: Integer);
    procedure InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);

    procedure ReverseCheckState(Node: PVirtualNode);

    function GetIsAllChecked: Boolean;
    function GetIsAllUnchecked: Boolean;

    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    function GetChecked(AIndex: Integer): Boolean;

    procedure Check(Node: PVirtualNode);
    procedure Check(const AIndex: Integer);
    procedure Uncheck(Node: PVirtualNode);
    procedure Uncheck(const AIndex: Integer);
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;
    procedure ValuesClear; override;
    procedure Draw; override;

    procedure CheckAll(const AChecked: Boolean);

    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
    property IsAllChecked: Boolean read GetIsAllChecked;
    property IsAllUnchecked: Boolean read GetIsAllUnchecked;
  end;

  { TVSTCategoryCustomTable }

  TVSTCategoryCustomTable = class(TVSTCoreTable)
  protected
    FCategoryValues: TStrVector;
    FDataValues: TStrMatrix3D;

    procedure HeaderClear; override;
    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; TextType: TVSTTextType;
                      var CellText: String);
    function IsIndexesCorrect(const AIndex1, AIndex2: Integer): Boolean;
    function NodeFromIndexes(const AIndex1, AIndex2: Integer): PVirtualNode;
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter);
    procedure SetColumn(const AColumnIndex: Integer; const AValues: TStrMatrix;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetColumn(const ACaption: String; const AValues: TStrMatrix;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetCategories(const AValues: TStrVector);

    procedure Clear;
    procedure ValuesClear; virtual;

    procedure Draw;
    procedure ExpandAll(const AExpand: Boolean);
    procedure Show(const AIndex1, AIndex2: Integer);
  end;

  { TVSTCategoryRadioButtonTable }

  TVSTCategoryRadioButtonTable = class(TVSTCategoryCustomTable)
  protected
    FSelectedIndex1, FSelectedIndex2: Integer;

    procedure InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        Shift: TShiftState; X, Y: Integer);
    procedure DrawText(Sender: TBaseVirtualTree;
                       TargetCanvas: TCanvas; Node: PVirtualNode;
                       Column: TColumnIndex;
                       const CellText: String; const CellRect: TRect;
                       var DefaultDraw: Boolean);
    procedure BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                              Node: PVirtualNode; Column: TColumnIndex;
                              CellPaintMode: TVTCellPaintMode; CellRect: TRect;
                              var ContentRect: TRect);

    procedure SetCanSelect(AValue: Boolean); override;
    procedure Select(Node: PVirtualNode);
    procedure UnselectNode;
    function GetIsSelected: Boolean;
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure Draw;

    procedure ValuesClear; override;

    procedure Select(const AIndex1, AIndex2: Integer);
    property CanSelect: Boolean read FCanSelect write SetCanSelect;
    property SelectedIndex1: Integer read FSelectedIndex1;
    property SelectedIndex2: Integer read FSelectedIndex2;
    property IsSelected: Boolean read GetIsSelected;
  end;

  { TVSTCategoryCheckTable }

  TVSTCategoryCheckTable = class(TVSTCategoryCustomTable)
  private
    procedure InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;
  end;

implementation

{ TVSTCategoryCheckTable }

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

constructor TVSTCategoryCheckTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.OnInitNode:= @InitNode;
end;

destructor TVSTCategoryCheckTable.Destroy;
begin
  inherited Destroy;
end;

{ TVSTCategoryRadioButtonTable }

procedure TVSTCategoryRadioButtonTable.InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if FTree.GetNodeLevel(Node)=1 then
  begin
    Node^.CheckType:= ctRadioButton;
    Node^.CheckState:= csUncheckedNormal;
  end;

  //Node^.CheckType:= ctCheckBox;
  //  Node^.CheckState:= csUncheckedNormal;
end;

function TVSTCategoryRadioButtonTable.GetIsSelected: Boolean;
begin
  Result:= (FSelectedIndex1>=0) and (FSelectedIndex2>=0);
end;

procedure TVSTCategoryRadioButtonTable.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Ind1, Ind2: Integer;
  Node: PVirtualNode;
begin
  if not FCanSelect then Exit;

  Node:= FTree.GetNodeAt(X, Y);
  if not Assigned(Node) then Exit;
  if FTree.GetNodeLevel(Node)<>1 then Exit;

  Ind1:= (Node^.Parent)^.Index;
  Ind2:= Node^.Index;
  if not IsIndexesCorrect(Ind1, Ind2) then Exit;

  if Button=mbRight then
    UnselectNode
  else if Button=mbLeft then
    Select(Node);

end;

procedure TVSTCategoryRadioButtonTable.DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  if Node=FSelectedNode then
    TargetCanvas.Font.Assign(FSelectedFont)
  else
    TargetCanvas.Font.Assign(FValuesFont);
end;

procedure TVSTCategoryRadioButtonTable.BeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  BGColor: TColor;
begin
  BGColor:= FValuesBGColor;
  if FSelectedNode=Node then
    BGColor:= FSelectedBGColor;
  if FDrawBorders then
    VSTCellDraw(FBorderColor, BGColor, TargetCanvas, Column, CellRect)
  else
    VSTCellDraw(BGColor, BGColor, TargetCanvas, Column, CellRect);
end;

procedure TVSTCategoryRadioButtonTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then UnselectNode;
  inherited SetCanSelect(AValue);
end;

procedure TVSTCategoryRadioButtonTable.Select(Node: PVirtualNode);
begin
  UnselectNode;
  FSelectedIndex1:= (Node^.Parent)^.Index;
  FSelectedIndex2:= Node^.Index;
  FSelectedNode:= Node;
  FTree.FocusedNode:= FSelectedNode;
  FSelectedNode^.CheckState:= csCheckedNormal;
  FTree.Refresh;
end;

procedure TVSTCategoryRadioButtonTable.UnselectNode;
begin
  if not IsSelected then Exit;
  if Assigned(FSelectedNode) then
    FSelectedNode^.CheckState:= csUncheckedNormal;
  FSelectedIndex1:= -1;
  FSelectedIndex2:= -1;
  FSelectedNode:= nil;
  FTree.Refresh;
end;

constructor TVSTCategoryRadioButtonTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.OnInitNode:= @InitNode;
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnDrawText:= @DrawText;
  FTree.OnBeforeCellPaint:= @BeforeCellPaint;
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
  Node:= NodeFromIndexes(AIndex1, AIndex2);
  if Assigned(Node) then Select(Node);
end;

{ TVSTCategoryCustomTable }

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

function TVSTCategoryCustomTable.NodeFromIndexes(const AIndex1, AIndex2: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result:= nil;
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if FTree.GetNodeLevel(Node)=1 then
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

constructor TVSTCategoryCustomTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  Clear;
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toCheckSupport];
  FTree.LineStyle:= lsSolid;
  FTree.OnGetText:= @GetText;
end;

destructor TVSTCategoryCustomTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTCategoryCustomTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
var
  Col: TVirtualTreeColumn;
begin
  VAppend(FHeaderCaptions, ACaption);
  VAppend(FColumnWidths, AWidth);
  MAppend(FDataValues, nil);
  Col:= FTree.Header.Columns.Add;
  Col.Text:= ACaption;
  Col.CaptionAlignment:= ACaptionAlignment;
  Col.Margin:= 3;  //!!!
  Col.Spacing:= 0; //!!!
  Col.Width:= AWidth;
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
  for i:=0 to High(FDataValues) do
    FDataValues[i]:= nil;
  FTree.Clear;
end;

procedure TVSTCategoryCustomTable.Draw;
var
  i, MaxLength: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;
  if MIsNil(FDataValues) then Exit;

  if VIsNil(FCategoryValues) then
  begin
    for i:= 0 to High(FDataValues[0]) do
      VAppend(FCategoryValues, 'Категория' + IntToStr(i+1));
  end;

  {ВЫРОВНЯТЬ МАТРИЦЫ!!!!!!}
  //MaxLength:= MMaxLength(FDataValues);
  //for i:= 0 to High(FDataValues) do
  //  if Length(FDataValues[i])<MaxLength then
  //    VReDim(FDataValues[i], MaxLength, EmptyStr);

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
  Node:= NodeFromIndexes(AIndex1, AIndex2);
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

procedure TVSTCoreTable.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetDrawBorders(AValue: Boolean);
begin
  if FDrawBorders=AValue then Exit;
  FDrawBorders:=AValue;
  FTree.Refresh;
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
  FTree.Header.AutoSizeIndex:= High(FHeaderCaptions);
  for i:= 0 to High(FHeaderCaptions) do
    FTree.Header.Columns[i].Width:= FColumnWidths[i];
end;

procedure TVSTCoreTable.SetDrawBorders;
begin

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
  if VIsNil(FHeaderCaptions) then
    VSTHeaderDraw(FTree.Color, FTree.Color, PaintInfo, Elements)
  else begin
    if FDrawBorders then
      VSTHeaderDraw(FBorderColor, FHeaderBGColor, PaintInfo, Elements)
    else
      VSTHeaderDraw(FHeaderBGColor, FHeaderBGColor, PaintInfo, Elements);
  end;
end;

constructor TVSTCoreTable.Create(const ATree: TVirtualStringTree);
begin
  FTree:= ATree;
  FHeaderFont:= TFont.Create;
  FValuesFont:= TFont.Create;
  FSelectedFont:= TFont.Create;
  FHeaderFont.Assign(FTree.Header.Font);
  FValuesFont.Assign(FTree.Font);
  FSelectedFont.Assign(FTree.Font);

  FDrawBorders:= True;
  FBorderColor:= clWindowText;
  FValuesBGColor:= clWindow;
  FHeaderBGColor:= FValuesBGColor;
  FSelectedBGColor:= clHighlight;

  FTree.Colors.GridLineColor:= FBorderColor;
  FTree.Color:= FValuesBGColor;

  FTree.DefaultNodeHeight:= 25;
  FTree.Header.DefaultHeight:= FTree.DefaultNodeHeight + 1;

  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions +
                                   [toAlwaysHideSelection, toHideFocusRect];

  FTree.Header.Options:= FTree.Header.Options +
                         [hoOwnerDraw, hoVisible, hoAutoResize];

  FTree.OnHeaderDrawQueryElements:= @HeaderDrawQueryElements;
  FTree.OnAdvancedHeaderDraw:= @AdvancedHeaderDraw;

end;

destructor TVSTCoreTable.Destroy;
begin
  FreeAndNil(FHeaderFont);
  FreeAndNil(FValuesFont);
  FreeAndNil(FSelectedFont);
  inherited Destroy;
end;

{ TVSTCheckTable }

procedure TVSTCheckTable.DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  if FChecked[Node^.Index] then
    TargetCanvas.Font.Assign(FSelectedFont)
  else
    TargetCanvas.Font.Assign(FValuesFont);
end;

procedure TVSTCheckTable.BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  BGColor: TColor;
begin
  BGColor:= FValuesBGColor;
  if FChecked[Node^.Index] then
    BGColor:= FSelectedBGColor;
  if FDrawBorders then
    VSTCellDraw(FBorderColor, BGColor, TargetCanvas, Column, CellRect)
  else
    VSTCellDraw(BGColor, BGColor, TargetCanvas, Column, CellRect);
end;

function TVSTCheckTable.GetIsAllUnchecked: Boolean;
begin
  Result:= VIsAllFalse(FChecked);
end;

function TVSTCheckTable.GetIsAllChecked: Boolean;
begin
  Result:= VIsAllTrue(FChecked);
end;

function TVSTCheckTable.GetChecked(AIndex: Integer): Boolean;
begin
  Result:= False;
  if not IsIndexCorrect(AIndex) then Exit;
  Result:= FChecked[AIndex];
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

procedure TVSTCheckTable.Check(Node: PVirtualNode);
begin
  if (not Assigned(Node)) or VIsNil(FChecked) then Exit;
  Node^.CheckState:= csCheckedNormal;
  FChecked[Node^.Index]:= True;
  FTree.Refresh;
end;

procedure TVSTCheckTable.Uncheck(Node: PVirtualNode);
begin
  if (not Assigned(Node)) or VIsNil(FChecked) then Exit;
  Node^.CheckState:= csUncheckedNormal;
  FChecked[Node^.Index]:= False;
  FTree.Refresh;
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
    Node:= FTree.GetNext(Node);
  end;
  VReDim(FChecked, Length(FDataValues[0]), AChecked);
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
    CheckAll(False)
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
  //FTree.Margin:= 0;
  FTree.Indent:= 0;



  FTree.OnDrawText:= @DrawText;
  FTree.OnBeforeCellPaint:= @BeforeCellPaint;
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
  FChecked:= nil;
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

function TVSTCustomTable.NodeFromIndex(const AIndex: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result:= nil;
  if not IsIndexCorrect(AIndex) then Exit;
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if Node^.Index = AIndex then
    begin
      Result:= Node;
      break;
    end;
    Node:= FTree.GetNext(Node);
  end;
end;

constructor TVSTCustomTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  Clear;
  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
  FTree.OnGetText:= @GetText;
end;

destructor TVSTCustomTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTCustomTable.Clear;
begin
  ValuesClear;
  HeaderClear;
end;

procedure TVSTCustomTable.ValuesClear;
var
  i: Integer;
begin
  for i:=0 to High(FDataValues) do
    FDataValues[i]:= nil;
  FTree.Clear;
end;

procedure TVSTCustomTable.Draw;
var
  i, MaxLength: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;

  MaxLength:= MMaxLength(FDataValues);
  for i:= 0 to High(FDataValues) do
    if Length(FDataValues[i])<MaxLength then
      VReDim(FDataValues[i], MaxLength, EmptyStr);
  VSTLoad(FTree, FDataValues[0]);

  SetColumnWidths;



end;

procedure TVSTCustomTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
var
  Col: TVirtualTreeColumn;
begin
  VAppend(FHeaderCaptions, ACaption);
  VAppend(FColumnWidths, AWidth);
  MAppend(FDataValues, nil);
  Col:= FTree.Header.Columns.Add;
  Col.Text:= ACaption;
  Col.CaptionAlignment:= ACaptionAlignment;
  Col.Margin:= 3;
  Col.Spacing:= 0;
  Col.Width:= AWidth;

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

{ TVSTTable }

procedure TVSTTable.Select(const AIndex: Integer);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(AIndex);
  if Assigned(Node) then Select(Node);
end;

procedure TVSTTable.Select(Node: PVirtualNode);
begin
  FSelectedIndex:= Node^.Index;
  FSelectedNode:= Node;
  FTree.FocusedNode:= FSelectedNode;
  FTree.Refresh;
end;

function TVSTTable.GetIsSelected: Boolean;
begin
  Result:= FSelectedIndex>=0;
end;

procedure TVSTTable.UnselectNode;
begin
  FSelectedIndex:= -1;
  FSelectedNode:= nil;
  FTree.Refresh;
end;

procedure TVSTTable.DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);
begin
  if Node=FSelectedNode then
    TargetCanvas.Font.Assign(FSelectedFont)
  else
    TargetCanvas.Font.Assign(FValuesFont);
end;

procedure TVSTTable.BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  BGColor: TColor;
begin
  BGColor:= FValuesBGColor;
  if FSelectedNode=Node then
    BGColor:= FSelectedBGColor;
  if FDrawBorders then
    VSTCellDraw(FBorderColor, BGColor, TargetCanvas, Column, CellRect)
  else
    VSTCellDraw(BGColor, BGColor, TargetCanvas, Column, CellRect);
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
    UnselectNode
  else if Button=mbLeft then
    Select(Node);
end;


procedure TVSTTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then UnselectNode;
  inherited SetCanSelect(AValue);
end;

constructor TVSTTable.Create(const ATree: TVirtualStringTree);
begin
  inherited Create(ATree);
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions - [toCheckSupport];
  FTree.OnDrawText:= @DrawText;
  FTree.OnBeforeCellPaint:= @BeforeCellPaint;
  FTree.OnMouseDown:= @MouseDown;
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

//procedure TVSTTable.Clear;   //
//begin
//  ValuesClear;
//  HeaderClear;
//end;

procedure TVSTTable.Draw;
begin
  UnselectNode;
  inherited Draw;
  FTree.Refresh;
end;



end.

