unit DK_VSTTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, VirtualTrees, DK_Vector, DK_Matrix, DK_BGRADrawer;

const
  COLOR_BG_DEFAULT = clWindow;
  COLOR_FONT_DEFAULT = clWindowText;
  COLOR_LINE_DEFAULT = clWindowText;

  ROW_HEIGHT_DEFAULT = 25;
  LAST_COLUMN_INDEX_FOR_AUTOSIZE = -2;

type
  TVSTColumnType = (
    ctUndefined,
    ctInteger,
    ctString,
    ctDate,
    ctTime,
    ctDouble,
    ctKeyPick,
    ctColor
  );
  TVSTColumnTypes = array of TVSTColumnType;

  TVSTEvent = procedure of object;
  TVSTRowCheckEvent = procedure(const ARowIndex: Integer; const AChecked: Boolean) of object;
  TVSTCellCheckEvent = procedure(const ARowIndex, AColIndex: Integer; const AChecked: Boolean) of object;
  TVSTEdititingDoneEvent = procedure(const ARowIndex, AColIndex: Integer;
                                     const ANewText: String;
                                     const AColumnType: TVSTColumnType;
                                     const ASaveChanges: Boolean) of object;

  procedure VSTLoad(const VST: TVirtualStringTree; const AVector: TStrVector);
  procedure VSTLoad(const VST: TVirtualStringTree; const AMatrix: TStrMatrix;
                    const AExpandNode: Boolean = True);
  procedure VSTLoad(const VST: TVirtualStringTree; const AMatrix: TStrMatrix;
                    const AExpandNodeIndex: Integer);

  procedure VSTNodeHeights(const VST: TVirtualStringTree;
                           const AHeight: Integer);
  function VSTShowNode(const VST: TVirtualStringTree;
                       const AInd1, AInd2: Integer): PVirtualNode;

  procedure VSTGetText(Node: PVirtualNode; var CellText: String;
                     const VST: TVirtualStringTree;
                     const AVector: TStrVector; const AMatrix: TStrMatrix);

  procedure VSTHeaderArrowDraw(const AArrowColor, ABackgroundColor: TColor;
                               const ARect: TRect;
                               const ACanvas: TCanvas;
                               const ADown: Boolean);
  procedure VSTHeaderDraw(const ABorderColor, ABackgroundColor: TColor;
                         var PaintInfo: THeaderPaintInfo;
                         const Elements: THeaderPaintElements);
  procedure VSTCellDraw(const ABorderColor, ABackgroundColor: TColor;
                       TargetCanvas: TCanvas; {%H-}Column: TColumnIndex;
                       CellRect: TRect; const ANeedTopLine: Boolean = False);

implementation

procedure VSTLoad(const VST: TVirtualStringTree; const AVector: TStrVector);
var
  i: Integer;
begin
  VST.Clear;
  for i:= 0 to High(AVector) do
    VST.AddChild(VST.RootNode);
end;

procedure VSTLoad(const VST: TVirtualStringTree; const AMatrix: TStrMatrix;
                 const AExpandNode: Boolean = True);
var
  i,j: Integer;
  Node: PVirtualNode;
begin
  VST.Clear;
  for i:= 0 to High(AMatrix) do
  begin
    Node := VST.AddChild(VST.RootNode);
    for j:= 0 to High(AMatrix[i]) do VST.AddChild(Node);
    VST.Expanded[Node]:= AExpandNode;
  end;
end;

procedure VSTLoad(const VST: TVirtualStringTree; const AMatrix: TStrMatrix;
                 const AExpandNodeIndex: Integer);
var
  i,j: Integer;
  Node: PVirtualNode;
begin
  VST.Clear;
  for i:= 0 to High(AMatrix) do
  begin
    Node := VST.AddChild(VST.RootNode);
    for j:= 0 to High(AMatrix[i]) do VST.AddChild(Node);
    VST.Expanded[Node]:= i=AExpandNodeIndex;
  end;
end;

procedure VSTNodeHeights(const VST: TVirtualStringTree; const AHeight: Integer);
var
  Node: PVirtualNode;
begin
  Node:= VST.GetFirst;
  while Assigned(Node) do
  begin
    if VST.GetNodeLevel(Node)=0 then
      Node^.NodeHeight:= AHeight;
    Node:= VST.GetNext(Node);
  end;
  VST.Refresh;
end;

function VSTShowNode(const VST: TVirtualStringTree; const AInd1, AInd2: Integer): PVirtualNode;
var
  i,j: Integer;
  Node: PVirtualNode;
begin
  Result:= nil;
  Node:= VST.GetFirst;
  while Assigned(Node) do
  begin
    if VST.GetNodeLevel(Node)=1 then
    begin
      i:= (Node^.Parent)^.Index;
      j:= Node^.Index;
      if (i=AInd1) and (j=AInd2) then
      begin
        VST.Expanded[Node^.Parent]:= True;
        VST.FocusedNode:= Node;
        Result:= Node;
        break;
      end;
    end;
    Node:= VST.GetNext(Node);
  end;
end;

procedure VSTGetText(Node: PVirtualNode; var CellText: String;
                     const VST: TVirtualStringTree;
                     const AVector: TStrVector; const AMatrix: TStrMatrix);
var
  i, j: Integer;
begin
  if VST.GetNodeLevel(Node)=0 then
  begin
    i:= Node^.Index;
    CellText:= AVector[i];
  end
  else begin
    i:= (Node^.Parent)^.Index;
    j:= Node^.Index;
    CellText:= AMatrix[i,j];
  end;
end;

procedure VSTHeaderArrowDraw(const AArrowColor, ABackgroundColor: TColor;
                             const ARect: TRect;
                             const ACanvas: TCanvas;
                             const ADown: Boolean);
var
  Drawer: TBGRADrawer;
  H: Integer;
begin
  H:= ARect.Height-4;

  Drawer:= TBGRADrawer.Create(8, H, ABackgroundColor);
  try
    Drawer.Line(4, 2, 4, H-2, AArrowColor, 2, psSolid);
    if ADown then
    begin
      Drawer.Line(1, H-5, 4, H-2, AArrowColor, 2, psSolid);
      Drawer.Line(7, H-5, 4, H-2, AArrowColor, 2, psSolid);
    end
    else begin
      Drawer.Line(1, 5, 4, 2, AArrowColor, 2, psSolid);
      Drawer.Line(7, 5, 4, 2, AArrowColor, 2, psSolid);
    end;
    Drawer.Draw(ACanvas, ARect.Right-10, ARect.Top+1, True);
  finally
    FreeAndNil(Drawer);
  end;
end;

procedure VSTHeaderDraw(const ABorderColor, ABackgroundColor: TColor;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var
  R: TRect;
begin
  if not (hpeBackground in Elements) then Exit;

  R:= PaintInfo.PaintRectangle;
  PaintInfo.TargetCanvas.Pen.Color:= ABorderColor;
  PaintInfo.TargetCanvas.Brush.Color:= ABackgroundColor;
  PaintInfo.TargetCanvas.Pen.Style:= psSolid;
  PaintInfo.TargetCanvas.Brush.Style:= bsSolid;

  if R.Left=0 then
    PaintInfo.TargetCanvas.Rectangle(R)
  else begin
    PaintInfo.TargetCanvas.FillRect(R);
    PaintInfo.TargetCanvas.MoveTo(R.Left, R.Top);
    PaintInfo.TargetCanvas.LineTo(R.Right-1, R.Top);
    PaintInfo.TargetCanvas.LineTo(R.Right-1, R.Bottom-1);
    PaintInfo.TargetCanvas.LineTo(R.Left-1, R.Bottom-1);
  end;

  //VSTHeaderArrowDraw(clHighlight, ABackgroundColor, R, PaintInfo.TargetCanvas, False);
end;

procedure VSTCellDraw(const ABorderColor, ABackgroundColor: TColor;
  TargetCanvas: TCanvas; Column: TColumnIndex; CellRect: TRect;
  const ANeedTopLine: Boolean = False);
var
  R: TRect;
begin
  R:= CellRect;
  TargetCanvas.Pen.Style:= psSolid;
  TargetCanvas.Pen.Color:= ABorderColor;
  TargetCanvas.Brush.Style:= bsSolid;
  TargetCanvas.Brush.Color:= ABackgroundColor;

  if R.Left>0 then
    R.Left:= R.Left-1;
  if not ANeedTopLine then
    R.Top:= R.Top-1;

  TargetCanvas.Rectangle(R);
end;

end.

