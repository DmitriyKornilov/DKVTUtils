unit DK_VSTTables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, VirtualTrees,
  DK_VSTUtils, DK_Vector, DK_Matrix;

type

  { TVSTCustomTable }

  TVSTCustomTable = class(TObject)
  protected
    FTree: TVirtualStringTree;

    FBorderColor: TColor;
    FValuesBGColor: TColor;
    FHeaderBGColor: TColor;
    FSelectedBGColor: TColor;

    FHeaderCaptions: TStrVector;
    FDataValues: TStrMatrix;
    FColumnWidths: TIntVector;

    FHeaderFont: TFont;
    FValuesFont: TFont;
    FSelectedFont: TFont;



    procedure HeaderClear;
    procedure HeaderDrawQueryElements(Sender: TVTHeader;
                            var PaintInfo: THeaderPaintInfo;
                            var Elements: THeaderPaintElements);
    procedure AdvancedHeaderDraw(Sender: TVTHeader;
                            var PaintInfo: THeaderPaintInfo;
                            const Elements: THeaderPaintElements);
    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; TextType: TVSTTextType;
                      var CellText: String);

    procedure SetHeaderBGColor(AValue: TColor);
    procedure SetHeaderFont(AValue: TFont);
    procedure SetSelectedBGColor(AValue: TColor);
    procedure SetSelectedFont(AValue: TFont);
    procedure SetValuesBGColor(AValue: TColor);
    procedure SetValuesFont(AValue: TFont);

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



    property ValuesBGColor: TColor read FValuesBGColor write SetValuesBGColor;
    property HeaderBGColor: TColor read FHeaderBGColor write SetHeaderBGColor;
    property SelectedBGColor: TColor read FSelectedBGColor write SetSelectedBGColor;



    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property ValuesFont: TFont read FValuesFont write SetValuesFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
  end;


  { TVSTTable }

  TVSTTable = class(TVSTCustomTable)
  private
    FSelectedIndex: Integer;
    FSelectedNode: PVirtualNode;
    FCanSelect: Boolean;

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

    procedure SetCanSelect(const AValue: Boolean);

  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    //procedure Clear;  //

    procedure ValuesClear; override;

    procedure Draw; override;

    function Select(const AIndex: Integer): Boolean;
    property SelectedIndex: Integer read FSelectedIndex;
    property IsSelected: Boolean read GetIsSelected;
    property CanSelect: Boolean read FCanSelect write SetCanSelect;
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
    procedure Check(Node: PVirtualNode);
    procedure Uncheck(Node: PVirtualNode);

  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;
    procedure ValuesClear; override;
    procedure Draw; override;

    procedure CheckAll(const AChecked: Boolean);
    procedure Check(const AIndex: Integer);
    procedure Uncheck(const AIndex: Integer);
  end;

implementation

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
  VSTCellDraw(FBorderColor, BGColor, TargetCanvas, Column, CellRect);
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
  //FTree.Refresh;
end;

procedure TVSTCheckTable.Uncheck(Node: PVirtualNode);
begin
  if (not Assigned(Node)) or VIsNil(FChecked) then Exit;
  Node^.CheckState:= csUncheckedNormal;
  FChecked[Node^.Index]:= False;
  //FTree.Refresh;
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
  FHeaderCaptions:= nil;
  FColumnWidths:= nil;
  FTree.Header.Columns.Clear;
  FDataValues:= nil;
end;

procedure TVSTCustomTable.HeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements:= [hpeBackground];
end;

procedure TVSTCustomTable.AdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  PaintInfo.TargetCanvas.Font.Assign(FHeaderFont);
  if VIsNil(FHeaderCaptions) then
    VSTHeaderDraw(FTree.Color, FTree.Color, PaintInfo, Elements)
  else
    VSTHeaderDraw(FBorderColor, FHeaderBGColor, PaintInfo, Elements);
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

procedure TVSTCustomTable.SetHeaderBGColor(AValue: TColor);
begin
  if FHeaderBGColor=AValue then Exit;
  FHeaderBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCustomTable.SetHeaderFont(AValue: TFont);
begin
  FHeaderFont.Assign(AValue);
  FTree.Header.Font.Assign(FHeaderFont);
  FTree.Refresh;
end;

procedure TVSTCustomTable.SetSelectedBGColor(AValue: TColor);
begin
  if FSelectedBGColor=AValue then Exit;
  FSelectedBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCustomTable.SetSelectedFont(AValue: TFont);
begin
  FSelectedFont.Assign(AValue);
  FTree.Refresh;
end;

procedure TVSTCustomTable.SetValuesBGColor(AValue: TColor);
begin
  if FValuesBGColor=AValue then Exit;
  FValuesBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCustomTable.SetValuesFont(AValue: TFont);
begin
  FValuesFont.Assign(AValue);
  FTree.Font.Assign(FValuesFont);
  FTree.Refresh;
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
  FTree:= ATree;
  FHeaderFont:= TFont.Create;
  FValuesFont:= TFont.Create;
  FSelectedFont:= TFont.Create;
  FHeaderFont.Assign(FTree.Header.Font);
  FValuesFont.Assign(FTree.Font);
  FSelectedFont.Assign(FTree.Font);

  Clear;

  FBorderColor:= clWindowText;
  FValuesBGColor:= clWindow;
  FHeaderBGColor:= FValuesBGColor;
  FSelectedBGColor:= clHighlight;

  FTree.Colors.GridLineColor:= FBorderColor;
  FTree.Color:= FValuesBGColor;

  FTree.DefaultNodeHeight:= 25;
  FTree.Header.DefaultHeight:= FTree.DefaultNodeHeight + 1;

  //FTree.Margin:= 0;
  //FTree.Indent:= 0;

  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions -
                                   [toShowTreeLines] +
                                   [toAlwaysHideSelection, toHideFocusRect];
  FTree.Header.Options:= FTree.Header.Options +
                         [hoOwnerDraw, hoVisible, hoAutoResize];


  FTree.OnHeaderDrawQueryElements:= @HeaderDrawQueryElements;
  FTree.OnAdvancedHeaderDraw:= @AdvancedHeaderDraw;
  FTree.OnGetText:= @GetText;


end;

destructor TVSTCustomTable.Destroy;
begin
  FreeAndNil(FHeaderFont);
  FreeAndNil(FValuesFont);
  FreeAndNil(FSelectedFont);
  inherited Destroy;
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

  FTree.Header.AutoSizeIndex:= High(FHeaderCaptions);
  for i:= 0 to High(FHeaderCaptions) do
    FTree.Header.Columns[i].Width:= FColumnWidths[i];


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

function TVSTTable.Select(const AIndex: Integer): Boolean;
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(AIndex);
  if Assigned(Node) then Select(Node);

  //Result:= False;
  //Node:= FTree.GetFirst;
  //while Assigned(Node) do
  //begin
  //  if Node^.Index = AIndex then
  //  begin
  //    Select(Node);
  //    Result:= True;
  //    break;
  //  end;
  //  Node:= FTree.GetNext(Node);
  //end;
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
  VSTCellDraw(FBorderColor, BGColor, TargetCanvas, Column, CellRect);
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


procedure TVSTTable.SetCanSelect(const AValue: Boolean);
begin
  if FCanSelect=AValue then Exit;
  if not AValue then UnselectNode;
  FCanSelect:=AValue;
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

