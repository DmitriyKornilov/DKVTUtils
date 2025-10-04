unit DK_VSTParamList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Forms, VirtualTrees,
  Graphics, DK_VSTTypes, DK_VSTTables, DK_VSTTableTools, DK_Vector;

const
  SCROLLBAR_WIDTH_DEFAULT = 16;
  SCROLLBAR_INCREMENT_DEFAULT = 20;

type
  TVSTListType = (ltString, ltCheck);

  { TVSTParamList }

  TVSTParamList = class (TObject)
  private
    FParent: TPanel;
    FTrees: array of TVirtualStringTree;
    FItems: array of TVSTCoreTable;
    FTypes: array of TVSTListType;
    FVisibles: TBoolVector;
    FNames: TStrVector;
    FScrollBox: TScrollBox;
    FSpace: Integer;
    FFont: TFont;
    FCaptionHeight: Integer;
    FItemHeight: Integer;

    function GetItem(const AItemName: String): TVSTCoreTable;
    function GetItemByIndex(const AItemIndex: Integer): TVSTCoreTable;
    function GetTree(const AItemName: String): TVirtualStringTree;
    function GetTreeByIndex(const AItemIndex: Integer): TVirtualStringTree;
    function ItemIndex(const AItemName: String): Integer;

    function GetIsSelectedByIndex(const AItemIndex: Integer): Boolean;
    function GetIsSelected(const AItemName: String): Boolean;
    function GetSelectedByIndex(const AItemIndex: Integer): Integer;
    function GetSelected(const AItemName: String): Integer;
    function GetCheckedByIndex(const AItemIndex, AParamIndex: Integer): Boolean;
    function GetChecked(const AItemName: String; const AParamIndex: Integer): Boolean;
    function GetCheckedsByIndex(const AItemIndex: Integer): TBoolVector;
    function GetCheckeds(const AItemName: String): TBoolVector;
    function GetCheckedsIntByIndex(const AItemIndex: Integer): TIntVector;
    function GetCheckedsInt(const AItemName: String): TIntVector;
    function GetVisiblesByIndex(const AItemIndex: Integer): Boolean;
    function GetVisibles(const AItemName: String): Boolean;
    function GetItemVisiblesByIndex(const AItemIndex: Integer): TBoolVector;
    function GetItemVisibles(const AItemName: String): TBoolVector;
    function GetParams: TIntVector;

    procedure SetSelectedByIndex(const AItemIndex: Integer; const AValue: Integer);
    procedure SetSelected(const AItemName: String; const AValue: Integer);
    procedure SetCheckedByIndex(const AItemIndex, AParamIndex: Integer; const AValue: Boolean);
    procedure SetChecked(const AItemName: String; const AParamIndex: Integer; const AValue: Boolean);
    procedure SetCheckedsByIndex(const AItemIndex: Integer; const AValues: TBoolVector);
    procedure SetCheckeds(const AItemName: String; const AValues: TBoolVector);
    procedure SetCheckedsIntByIndex(const AItemIndex: Integer; const AValues: TIntVector);
    procedure SetCheckedsInt(const AItemName: String; const AValues: TIntVector);
    procedure SetVisiblesByIndex(const AItemIndex: Integer; const AValue: Boolean);
    procedure SetVisibles(const AItemName: String; const AValue: Boolean);
    procedure SetItemVisiblesByIndex(const AItemIndex: Integer; const AValues: TBoolVector);
    procedure SetItemVisibles(const AItemName: String; const AValues: TBoolVector);
    procedure SetParams(const AValues: TIntVector);

    procedure AddCustomList(const AListType: TVSTListType; const AName: String);

    procedure MouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer;
                         {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
  public
    constructor Create(const AParent: TPanel; const AFont: TFont = nil);
    destructor Destroy; override;

    procedure AddStringList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ASelectedIndex: Integer = 0);
    procedure AddCheckList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ACheckeds: TBoolVector = nil);

    procedure AutoHeight;

    property IsSelectedByIndex[const AItemIndex: Integer]: Boolean read GetIsSelectedByIndex;
    property SelectedByIndex[const AItemIndex: Integer]: Integer read GetSelectedByIndex write SetSelectedByIndex;
    property CheckedByIndex[const AItemIndex, AParamIndex: Integer]: Boolean read GetCheckedByIndex write SetCheckedByIndex;
    property CheckedsByIndex[const AItemIndex: Integer]: TBoolVector read GetCheckedsByIndex write SetCheckedsByIndex;
    property CheckedsIntByIndex[const AItemIndex: Integer]: TIntVector read GetCheckedsIntByIndex write SetCheckedsIntByIndex;
    property VisiblesByIndex[const AItemIndex: Integer]: Boolean read GetVisiblesByIndex write SetVisiblesByIndex;
    property ItemVisiblesByIndex[const AItemIndex: Integer]: TBoolVector read GetItemVisiblesByIndex write SetItemVisiblesByIndex;

    property IsSelected[const AItemName: String]: Boolean read GetIsSelected;
    property Selected[const AItemName: String]: Integer read GetSelected write SetSelected;
    property Checked[const AItemName: String; const AParamIndex: Integer]: Boolean read GetChecked write SetChecked;
    property Checkeds[const AItemName: String]: TBoolVector read GetCheckeds write SetCheckeds;
    property CheckedsInt[const AItemName: String]: TIntVector read GetCheckedsInt write SetCheckedsInt;
    property Visibles[const AItemName: String]: Boolean read GetVisibles write SetVisibles;
    property ItemVisibles[const AItemName: String]: TBoolVector read GetItemVisibles write SetItemVisibles;
    property Params: TIntVector read GetParams write SetParams;

    property TreeByIndex[const AItemIndex: Integer]: TVirtualStringTree read GetTreeByIndex;
    property ItemByIndex[const AItemIndex: Integer]: TVSTCoreTable read GetItemByIndex;

    property Tree[const AItemName: String]: TVirtualStringTree read GetTree;
    property Item[const AItemName: String]: TVSTCoreTable read GetItem;
  end;

implementation

{ TVSTParamList }

function TVSTParamList.GetItemVisiblesByIndex(const AItemIndex: Integer): TBoolVector;
begin
  Result:= FItems[AItemIndex].Visibles;
end;

function TVSTParamList.GetItemVisibles(const AItemName: String): TBoolVector;
begin
  Result:= ItemVisiblesByIndex[ItemIndex(AItemName)];
end;

procedure TVSTParamList.SetItemVisiblesByIndex(const AItemIndex: Integer; const AValues: TBoolVector);
begin
  FItems[AItemIndex].Visibles:= AValues;
end;

procedure TVSTParamList.SetItemVisibles(const AItemName: String; const AValues: TBoolVector);
begin
  ItemVisiblesByIndex[ItemIndex(AItemName)]:= AValues;
end;

function TVSTParamList.ItemIndex(const AItemName: String): Integer;
begin
  Result:= VIndexOf(FNames, AItemName);
end;

function TVSTParamList.GetItem(const AItemName: String): TVSTCoreTable;
begin
  Result:= FItems[ItemIndex(AItemName)];
end;

function TVSTParamList.GetItemByIndex(const AItemIndex: Integer): TVSTCoreTable;
begin
  Result:= FItems[AItemIndex];
end;

function TVSTParamList.GetTree(const AItemName: String): TVirtualStringTree;
begin
  Result:= FTrees[ItemIndex(AItemName)];
end;

function TVSTParamList.GetTreeByIndex(const AItemIndex: Integer): TVirtualStringTree;
begin
  Result:= FTrees[AItemIndex];
end;

function TVSTParamList.GetVisibles(const AItemName: String): Boolean;
begin
  Result:= VisiblesByIndex[ItemIndex(AItemName)];
end;

function TVSTParamList.GetIsSelectedByIndex(const AItemIndex: Integer): Boolean;
begin
  Result:= FItems[AItemIndex].IsSelected;
end;

function TVSTParamList.GetIsSelected(const AItemName: String): Boolean;
begin
  Result:= IsSelectedByIndex[ItemIndex(AItemName)];
end;

function TVSTParamList.GetCheckedsIntByIndex(const AItemIndex: Integer): TIntVector;
begin
  Result:= VBoolToInt(CheckedsByIndex[AItemIndex]);
end;

function TVSTParamList.GetCheckedsInt(const AItemName: String): TIntVector;
begin
  Result:= VBoolToInt(Checkeds[AItemName]);
end;

procedure TVSTParamList.SetCheckedsIntByIndex(const AItemIndex: Integer; const AValues: TIntVector);
begin
  CheckedsByIndex[AItemIndex]:= VIntToBool(AValues);
end;

procedure TVSTParamList.SetCheckedsInt(const AItemName: String; const AValues: TIntVector);
begin
  Checkeds[AItemName]:= VIntToBool(AValues);
end;

procedure TVSTParamList.SetVisibles(const AItemName: String; const AValue: Boolean);
begin
  VisiblesByIndex[ItemIndex(AItemName)]:= AValue;
end;

function TVSTParamList.GetParams: TIntVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(FNames) then Exit;
  for i:= 0 to High(FNames) do
  begin
    case FTypes[i] of
      ltString: VAppend(Result, SelectedByIndex[i]);
      ltCheck:  Result:= VAdd(Result, CheckedsIntByIndex[i]);
    end;
  end;
end;

function TVSTParamList.GetVisiblesByIndex(const AItemIndex: Integer): Boolean;
begin
  Result:= FVisibles[AItemIndex];
end;

procedure TVSTParamList.SetVisiblesByIndex(const AItemIndex: Integer; const AValue: Boolean);
begin
  if FVisibles[AItemIndex]=AValue then Exit;
  FVisibles[AItemIndex]:= AValue;
  FTrees[AItemIndex].Visible:= AValue;
end;

function TVSTParamList.GetCheckedByIndex(const AItemIndex, AParamIndex: Integer): Boolean;
begin
  Result:= False;
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltCheck) then Exit;
  Result:= (FItems[AItemIndex] as TVSTCheckList).Checked[AParamIndex];
end;

function TVSTParamList.GetChecked(const AItemName: String; const AParamIndex: Integer): Boolean;
begin
  Result:= CheckedByIndex[ItemIndex(AItemName), AParamIndex];
end;

function TVSTParamList.GetCheckedsByIndex(const AItemIndex: Integer): TBoolVector;
begin
  Result:= nil;
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltCheck) then Exit;
  Result:= (FItems[AItemIndex] as TVSTCheckList).Checkeds;
end;

function TVSTParamList.GetCheckeds(const AItemName: String): TBoolVector;
begin
  Result:= CheckedsByIndex[ItemIndex(AItemName)];
end;

procedure TVSTParamList.SetCheckedByIndex(const AItemIndex, AParamIndex: Integer; const AValue: Boolean);
begin
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltCheck) then Exit;
  (FItems[AItemIndex] as TVSTCheckList).Checked[AParamIndex]:= AValue;
end;

procedure TVSTParamList.SetChecked(const AItemName: String; const AParamIndex: Integer; const AValue: Boolean);
begin
  CheckedByIndex[ItemIndex(AItemName), AParamIndex]:= AValue;
end;

procedure TVSTParamList.SetCheckedsByIndex(const AItemIndex: Integer; const AValues: TBoolVector);
begin
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltCheck) then Exit;
  (FItems[AItemIndex] as TVSTCheckList).Checkeds:= AValues;
end;

procedure TVSTParamList.SetCheckeds(const AItemName: String; const AValues: TBoolVector);
begin
  CheckedsByIndex[ItemIndex(AItemName)]:= AValues;
end;

function TVSTParamList.GetSelectedByIndex(const AItemIndex: Integer): Integer;
begin
  Result:= -1;
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltString) then Exit;
  Result:= (FItems[AItemIndex] as TVSTStringList).ItemIndex;
end;

function TVSTParamList.GetSelected(const AItemName: String): Integer;
begin
  Result:= SelectedByIndex[ItemIndex(AItemName)];
end;

procedure TVSTParamList.SetSelectedByIndex(const AItemIndex: Integer; const AValue: Integer);
begin
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltString) then Exit;
  (FItems[AItemIndex] as TVSTStringList).ItemIndex:= AValue;
end;

procedure TVSTParamList.SetSelected(const AItemName: String; const AValue: Integer);
begin
  SelectedByIndex[ItemIndex(AItemName)]:= AValue;
end;

procedure TVSTParamList.SetParams(const AValues: TIntVector);
var
  i, j, n: Integer;
begin
  if VIsNil(FNames) then Exit;
  j:= 0;
  for i:= 0 to High(FNames) do
  begin
    case FTypes[i] of
      ltString:
        begin
          n:= 1;
          SelectedByIndex[i]:= AValues[j];
        end;
      ltCheck:
        begin
          n:= (FItems[i] as TVSTCheckList).RowCount;
          CheckedsIntByIndex[i]:= VCut(AValues, j, j+n-1);
        end;
    end;
    j:= j + n;
  end;
end;

procedure TVSTParamList.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  H: Integer;
begin
  H:= FParent.Scale96ToForm(SCROLLBAR_INCREMENT_DEFAULT);
  if WheelDelta<0 then
    FScrollBox.VertScrollBar.Position:= FScrollBox.VertScrollBar.Position + H
  else
    FScrollBox.VertScrollBar.Position:= FScrollBox.VertScrollBar.Position - H;
end;

constructor TVSTParamList.Create(const AParent: TPanel; const AFont: TFont = nil);
var
  PPI: Integer;
begin
  inherited Create;

  FParent:= AParent;

  PPI:= Screen.PixelsPerInch;
  if PPI<108 then
    FSpace:= 0
  else if PPI<132 then
    FSpace:= 4
  else if PPI<156 then
    FSpace:= 8
  else
    FSpace:= 12;

  FCaptionHeight:= ROW_HEIGHT_DEFAULT;
  FItemHeight:= ROW_HEIGHT_DEFAULT;
  if PPI<108 then
  begin
    FItemHeight:= Round(1.25*FItemHeight);
    FCaptionHeight:= Round(1.25*FCaptionHeight);
  end;

  FFont:= TFont.Create;
  if Assigned(AFont) then
    FFont.Assign(AFont)
  else
    FFont.Assign(FParent.Font);

  FScrollBox:= TScrollBox.Create(nil);
  FScrollBox.Parent:= FParent;
  FScrollBox.Align:= alClient;
  FScrollBox.Color:= clWindow;
  FScrollBox.BorderStyle:= bsNone;
  FScrollBox.VertScrollBar.Increment:= FParent.Scale96ToForm(SCROLLBAR_INCREMENT_DEFAULT);
  FScrollBox.HorzScrollBar.Visible:= False;
end;

destructor TVSTParamList.Destroy;
var
  i: Integer;
begin
  for i:= 0 to High(FItems) do
    FreeAndNil(FItems[i]);
  FreeAndNil(FScrollBox);
  FreeAndNil(FFont);

  inherited Destroy;
end;

procedure TVSTParamList.AddCustomList(const AListType: TVSTListType; const AName: String);
var
  VT: TVirtualStringTree;
  N: Integer;
begin
  N:= Length(FTrees);
  SetLength(FTrees, N+1);

  VT:= TVirtualStringTree.Create(FScrollBox);
  VT.Parent:= FScrollBox;
  if N=0 then
  begin
    VT.AnchorSide[akTop].Side:= asrTop;
    VT.AnchorSide[akTop].Control:= FScrollBox;
  end
  else begin
    VT.AnchorSide[akTop].Side:= asrBottom;
    VT.AnchorSide[akTop].Control:= FTrees[N-1];
  end;
  VT.BorderSpacing.Bottom:= FSpace;
  VT.AnchorSide[akLeft].Side:= asrLeft;
  VT.AnchorSide[akLeft].Control:= FScrollBox;
  VT.AnchorSide[akRight].Side:= asrRight;
  VT.AnchorSide[akRight].Control:= FScrollBox;
  VT.Anchors:= [akLeft, akTop, akRight];

  VT.ScrollBarOptions.ScrollBars:= ssNone;
  VT.OnMouseWheel:= @MouseWheel;
  FTrees[N]:= VT;

  SetLength(FTypes, N+1);
  FTypes[N]:= AListType;
  VAppend(FNames, AName);
  VAppend(FVisibles, True);
end;

procedure TVSTParamList.AddStringList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ASelectedIndex: Integer = 0);
var
  N: Integer;
  List: TVSTStringList;
begin
  AddCustomList(ltString, AName);

  N:= Length(FItems);
  SetLength(FItems, N+1);
  List:= TVSTStringList.Create(FTrees[N], ACaption, AOnSelect, FCaptionHeight, FItemHeight);
  List.Update(AItems, ASelectedIndex);
  FItems[N]:= List;
end;

procedure TVSTParamList.AddCheckList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ACheckeds: TBoolVector = nil);
var
  N: Integer;
  List: TVSTCheckList;
begin
  AddCustomList(ltCheck, AName);

  N:= Length(FItems);
  SetLength(FItems, N+1);
  List:= TVSTCheckList.Create(FTrees[N], ACaption, AOnSelect, FCaptionHeight, FItemHeight);
  List.StopSelectEventWhileCheckAll:= True;
  List.Update(AItems, ACheckeds);
  FItems[N]:= List;
end;

procedure TVSTParamList.AutoHeight;
var
  i: Integer;
begin
  for i:= 0 to High(FItems) do
    FTrees[i].Height:= FItems[i].AutoHeightValue;
end;

end.

