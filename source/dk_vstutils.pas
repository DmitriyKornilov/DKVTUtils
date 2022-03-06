unit DK_VSTUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, DK_Vector, DK_Matrix;

  procedure VSTLoadVector(const VST: TVirtualStringTree; const ALength: Integer);
  procedure VSTLoadMatrix(const VST: TVirtualStringTree; const ALengths: TIntVector;
                          const AExpandNodes: Boolean = True);
  procedure VSTLoadMatrix(const VST: TVirtualStringTree; const ALengths: TIntVector;
                          const AExpandNodeIndex: Integer);

  function VSTShowMatrixNode(const VST: TVirtualStringTree;
                       const AInd1, AInd2: Integer): PVirtualNode;

implementation

procedure VSTLoadVector(const VST: TVirtualStringTree; const ALength: Integer);
var
  i: Integer;
begin
  VST.Clear;
  for i:= 0 to ALength-1 do
    VST.AddChild(VST.RootNode);
end;

procedure VSTLoadMatrix(const VST: TVirtualStringTree; const ALengths: TIntVector;
                        const AExpandNodes: Boolean);
var
  i,j: Integer;
  Node: PVirtualNode;
begin
  VST.Clear;
  for i:= 0 to High(ALengths) do
  begin
    Node := VST.AddChild(VST.RootNode);
    for j:= 0 to ALengths[i]-1 do VST.AddChild(Node);
    VST.Expanded[Node]:= AExpandNode;
  end;
end;

procedure VSTLoadMatrix(const VST: TVirtualStringTree; const ALengths: TIntVector;
                        const AExpandNodeIndex: Integer);
var
  i,j: Integer;
  Node: PVirtualNode;
begin
  VST.Clear;
  for i:= 0 to High(ALengths) do
  begin
    Node := VST.AddChild(VST.RootNode);
    for j:= 0 to ALengths[i]-1 do VST.AddChild(Node);
    VST.Expanded[Node]:= i=AExpandNodeIndex;
  end;
end;

function VSTShowMatrixNode(const VST: TVirtualStringTree;
                     const AInd1, AInd2: Integer): PVirtualNode;
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

end.

