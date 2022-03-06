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

end.

