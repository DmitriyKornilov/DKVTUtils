unit DK_VSTUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, DK_Vector, DK_Matrix;

  procedure VSTLoad(const VST: TVirtualStringTree; const AVector: TStrVector);
  procedure VSTLoad(const VST: TVirtualStringTree; const AMatrix: TStrMatrix;
                    const AExpandNode: Boolean = True);
  procedure VSTLoad(const VST: TVirtualStringTree; const AMatrix: TStrMatrix;
                    const AExpandNodeIndex: Integer);

  function VSTShowNode(const VST: TVirtualStringTree;
                       const AInd1, AInd2: Integer): PVirtualNode;

  procedure VSTGetText(Node: PVirtualNode; var CellText: String;
                     const VST: TVirtualStringTree;
                     const AVector: TStrVector; const AMatrix: TStrMatrix);


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

end.

