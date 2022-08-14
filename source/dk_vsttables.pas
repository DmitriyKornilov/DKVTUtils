unit DK_VSTTables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, VirtualTrees,
  DK_VSTUtils, DK_Vector, DK_Matrix;

type

  { TVSTTable }

  TVSTTable = class(TObject)
  private
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

    FCanSelect: Boolean;
    FSelectedIndex: Integer;
    FSelectedNode: PVirtualNode;

    procedure SelectNode(Node: PVirtualNode);
    procedure UnselectNode;


    procedure HeaderDrawQueryElements(Sender: TVTHeader;
                            var PaintInfo: THeaderPaintInfo;
                            var Elements: THeaderPaintElements);
    procedure AdvancedHeaderDraw(Sender: TVTHeader;
                            var PaintInfo: THeaderPaintInfo;
                            const Elements: THeaderPaintElements);
    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; TextType: TVSTTextType;
                      var CellText: String);
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
    procedure SetHeaderBGColor(const AValue: TColor);
    procedure SetValuesBGColor(const AValue: TColor);
    procedure SetSelectedBGColor(const AValue: TColor);

    procedure SetHeaderFont(const AValue: TFont);
    procedure SetValuesFont(const AValue: TFont);
    procedure SetSelectedFont(const AValue: TFont);
  public
    constructor Create(const ATree: TVirtualStringTree);
    destructor  Destroy; override;

    procedure Clear;
    procedure Draw;

    procedure ColumnAdd(const ACaption: String; const AValues: TStrVector;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);

    property ValuesBGColor: TColor read FValuesBGColor write SetValuesBGColor;
    property HeaderBGColor: TColor read FHeaderBGColor write SetHeaderBGColor;
    property SelectedBGColor: TColor read FSelectedBGColor write SetSelectedBGColor;

    property CanSelect: Boolean read FCanSelect write SetCanSelect;

    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property ValuesFont: TFont read FValuesFont write SetValuesFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;

  end;

implementation

{ TVSTTable }

procedure TVSTTable.SelectNode(Node: PVirtualNode);
begin
  FSelectedIndex:= Node^.Index;
  FSelectedNode:= Node;
  //SelectedNode^.CheckState:= csCheckedNormal;
  FTree.FocusedNode:= FSelectedNode;
  FTree.Refresh;
end;

procedure TVSTTable.SetSelectedFont(const AValue: TFont);
begin
  if FSelectedFont=AValue then Exit;
  FSelectedFont:= AValue;
  FTree.Refresh;
end;

procedure TVSTTable.SetHeaderFont(const AValue: TFont);
begin
  if FHeaderFont=AValue then Exit;
  FHeaderFont:= AValue;
  FTree.Header.Font.Assign(FHeaderFont);
  FTree.Refresh;
end;

procedure TVSTTable.SetValuesFont(const AValue: TFont);
begin
  if FValuesFont=AValue then Exit;
  FValuesFont:= AValue;
  FTree.Font.Assign(FValuesFont);
  FTree.Refresh;
end;


procedure TVSTTable.UnselectNode;
begin
  //if SelectedIndex>=0 then
  //  SelectedNode^.CheckState:= csUnCheckedNormal;
  FSelectedIndex:= -1;
  FSelectedNode:= nil;
  FTree.Refresh;
end;



procedure TVSTTable.HeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements:= [hpeBackground];
end;

procedure TVSTTable.AdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  PaintInfo.TargetCanvas.Font.Assign(FHeaderFont);
  VSTHeaderDraw(FBorderColor, FHeaderBGColor, PaintInfo, Elements);
end;

procedure TVSTTable.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  i: Integer;
begin
  if High(FDataValues)<Column then Exit;
  i:= Node^.Index;
  CellText:= FDataValues[Column, i];
end;

procedure TVSTTable.DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);
begin
  if Node=FSelectedNode then
    TargetCanvas.Font.Assign(FSelectedFont)
  else
    TargetCanvas.Font.Assign(FValuesFont)
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
  Node: PVirtualNode;
begin
  if not FCanSelect then Exit;
  if Button=mbRight then
    UnselectNode;

  if Button=mbLeft then
  begin
    Node:= FTree.GetNodeAt(X, Y);
    if Assigned(Node) then
      SelectNode(Node);
  end;

end;

procedure TVSTTable.SetCanSelect(const AValue: Boolean);
begin
  if FCanSelect=AValue then Exit;
  if not AValue then
    UnselectNode;
  FCanSelect:= AValue;
end;

procedure TVSTTable.SetHeaderBGColor(const AValue: TColor);
begin
  if FHeaderBGColor=AValue then Exit;
  FHeaderBGColor:= AValue;
  FTree.Refresh;
end;

procedure TVSTTable.SetValuesBGColor(const AValue: TColor);
begin
  if FValuesBGColor=AValue then Exit;
  FValuesBGColor:= AValue;
  FTree.Refresh;
end;

procedure TVSTTable.SetSelectedBGColor(const AValue: TColor);
begin
  if FSelectedBGColor=AValue then Exit;
  FSelectedBGColor:= AValue;
  FTree.Refresh;
end;

constructor TVSTTable.Create(const ATree: TVirtualStringTree);
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
  FTree.Margin:= 0;
  FTree.Indent:= 0;

  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions -
                                   [toShowTreeLines] +
                                   [toAlwaysHideSelection, toHideFocusRect];
  FTree.Header.Options:= FTree.Header.Options +
                         [hoOwnerDraw, hoVisible, hoAutoResize];
  FTree.OnHeaderDrawQueryElements:= @HeaderDrawQueryElements;
  FTree.OnAdvancedHeaderDraw:= @AdvancedHeaderDraw;

  FTree.OnGetText:= @GetText;
  FTree.OnDrawText:= @DrawText;
  FTree.OnBeforeCellPaint:= @BeforeCellPaint;
  FTree.OnMouseDown:= @MouseDown;

end;

destructor TVSTTable.Destroy;
begin
  FreeAndNil(FHeaderFont);
  FreeAndNil(FValuesFont);
  FreeAndNil(FSelectedFont);
  inherited Destroy;
end;

procedure TVSTTable.Clear;
begin
  FTree.Clear;
  FHeaderCaptions:= nil;
  FDataValues:= nil;
  FColumnWidths:= nil;
  FSelectedIndex:= -1;
end;

procedure TVSTTable.Draw;
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

  FTree.Refresh;
end;

procedure TVSTTable.ColumnAdd(const ACaption: String; const AValues: TStrVector;
                              const AWidth: Integer = 100;
                              const ACaptionAlignment: TAlignment = taCenter;
                              const AValuesAlignment: TAlignment = taCenter);
var
  Col: TVirtualTreeColumn;
begin
  VAppend(FHeaderCaptions, ACaption);
  MAppend(FDataValues, AValues);
  VAppend(FColumnWidths, AWidth);
  Col:= FTree.Header.Columns.Add;
  Col.Text:= ACaption;
  Col.CaptionAlignment:= ACaptionAlignment;
  Col.Alignment:= AValuesAlignment;
  Col.Margin:= 3;
  Col.Spacing:= 0;
  Col.Width:= AWidth;
end;

end.

