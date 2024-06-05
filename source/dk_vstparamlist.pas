unit DK_VSTParamList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, Forms, VirtualTrees,
  Graphics, DK_VSTTypes, DK_VSTTables, DK_VSTTableTools, DK_Vector;

const
  SCROLLBAR_WIDTH_DEFAULT = 16;
  SCROLLBAR_DELTA_DEFAULT = 20;
  //ITEM_SPACE_DEFAULT = 0;

type
  TVSTListType = (ltString, ltCheck);

  { TVSTParamList }

  TVSTParamList = class (TObject)
  private
    FParent: TPanel;
    FPanel: TPanel;
    FTrees: array of TVirtualStringTree;
    FItems: array of TVSTSimpleTable;
    FTypes: array of TVSTListType;
    FNames: TStrVector;
    FScrollBar: TScrollBar;
    FSpace: Integer;
    FHeight: Integer;
    FFont: TFont;

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
    function GetParams: TIntVector;

    procedure SetSelectedByIndex(const AItemIndex: Integer; const AValue: Integer);
    procedure SetSelected(const AItemName: String; const AValue: Integer);
    procedure SetCheckedByIndex(const AItemIndex, AParamIndex: Integer; const AValue: Boolean);
    procedure SetChecked(const AItemName: String; const AParamIndex: Integer; const AValue: Boolean);
    procedure SetCheckedsByIndex(const AItemIndex: Integer; const AValue: TBoolVector);
    procedure SetCheckeds(const AItemName: String; const AValue: TBoolVector);
    procedure SetCheckedsIntByIndex(const AItemIndex: Integer; const AValue: TIntVector);
    procedure SetCheckedsInt(const AItemName: String; const AValue: TIntVector);
    procedure SetParams(const AValue: TIntVector);

    procedure MouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer;
                         {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure ScrollBarChange(Sender: TObject);
    procedure ChangeBounds(Sender: TObject);
    procedure ResizeControls;
    procedure AddCustomList(const AListType: TVSTListType; const AName: String);

    property IsSelectedByIndex[const AItemIndex: Integer]: Boolean read GetIsSelectedByIndex;
    property SelectedByIndex[const AItemIndex: Integer]: Integer read GetSelectedByIndex write SetSelectedByIndex;
    property CheckedByIndex[const AItemIndex, AParamIndex: Integer]: Boolean read GetCheckedByIndex write SetCheckedByIndex;
    property CheckedsByIndex[const AItemIndex: Integer]: TBoolVector read GetCheckedsByIndex write SetCheckedsByIndex;
    property CheckedsIntByIndex[const AItemIndex: Integer]: TIntVector read GetCheckedsIntByIndex write SetCheckedsIntByIndex;

  public
    constructor Create(const AParent: TPanel; const AFont: TFont = nil);
    destructor Destroy; override;

    procedure Show; //use in TForm.OnShow

    procedure AddStringList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ASelectedIndex: Integer = 0);
    procedure AddCheckList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ACheckedCount: Integer = -1); //-1 check all, >=0 check [0..ACheckedCount-1]

    property IsSelected[const AItemName: String]: Boolean read GetIsSelected;
    property Selected[const AItemName: String]: Integer read GetSelected write SetSelected;
    property Checked[const AItemName: String; const AParamIndex: Integer]: Boolean read GetChecked write SetChecked;
    property Checkeds[const AItemName: String]: TBoolVector read GetCheckeds write SetCheckeds;
    property CheckedsInt[const AItemName: String]: TIntVector read GetCheckedsInt write SetCheckedsInt;
    property Params: TIntVector read GetParams write SetParams;

    property Height: Integer read FHeight;
  end;

implementation

{ TVSTParamList }

function TVSTParamList.ItemIndex(const AItemName: String): Integer;
begin
  Result:= VIndexOf(FNames, AItemName);
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

procedure TVSTParamList.SetCheckedsIntByIndex(const AItemIndex: Integer; const AValue: TIntVector);
begin
  CheckedsByIndex[AItemIndex]:= VIntToBool(AValue);
end;

procedure TVSTParamList.SetCheckedsInt(const AItemName: String; const AValue: TIntVector);
begin
  Checkeds[AItemName]:= VIntToBool(AValue);
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

procedure TVSTParamList.SetParams(const AValue: TIntVector);
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
          SelectedByIndex[i]:= AValue[j];
        end;
      ltCheck:
        begin
          n:= (FItems[i] as TVSTCheckList).Count;
          CheckedsIntByIndex[i]:= VCut(AValue, j, j+n-1);
        end;
    end;
    j:= j + n;
  end;
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
  Result:= (FItems[AItemIndex] as TVSTCheckList).Selected;
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

procedure TVSTParamList.SetCheckedsByIndex(const AItemIndex: Integer; const AValue: TBoolVector);
begin
  if (AItemIndex<0) or (FTypes[AItemIndex]<>ltCheck) then Exit;
  (FItems[AItemIndex] as TVSTCheckList).Selected:= AValue;
end;

procedure TVSTParamList.SetCheckeds(const AItemName: String; const AValue: TBoolVector);
begin
  CheckedsByIndex[ItemIndex(AItemName)]:= AValue;
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

procedure TVSTParamList.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  H: Integer;
begin
  if not FScrollBar.Visible then Exit;
  H:= FParent.Scale96ToForm(SCROLLBAR_DELTA_DEFAULT);
  if WheelDelta<0 then
    FScrollBar.Position:= FScrollBar.Position + H
  else
    FScrollBar.Position:= FScrollBar.Position - H;
end;

procedure TVSTParamList.ScrollBarChange(Sender: TObject);
begin
  FPanel.Top:= -FScrollBar.Position;
end;

procedure TVSTParamList.ChangeBounds(Sender: TObject);
begin
  ResizeControls;
end;

procedure TVSTParamList.ResizeControls;
var
  i: Integer;
begin
  FScrollBar.Visible:= FParent.ClientHeight<FHeight;
  if FScrollBar.Visible then
    FScrollBar.Max:= FHeight - FParent.ClientHeight
  else
    FScrollBar.Position:= 0;

  FPanel.Width:= FParent.ClientWidth - FScrollBar.Width*Ord(FScrollBar.Visible);
  for i:= 0 to High(FTrees) do
    FTrees[i].Width:= FPanel.ClientWidth;
end;

constructor TVSTParamList.Create(const AParent: TPanel; const AFont: TFont = nil);
begin
  inherited Create;

  FParent:= AParent;
  FParent.OnChangeBounds:= @ChangeBounds;
  FHeight:= 0;
  case Screen.PixelsPerInch of
    96 : FSpace:= 0;
    120: FSpace:= 4;
    144: FSpace:= 8;
    168: FSpace:= 12;
  end;

  FFont:= TFont.Create;
  if Assigned(AFont) then
    FFont.Assign(AFont)
  else
    FFont.Assign(FParent.Font);

  FScrollBar:= TScrollBar.Create(nil);
  FScrollBar.OnChange:= @ScrollBarChange;
  FScrollBar.Parent:= FParent;
  FScrollBar.Kind:= sbVertical;
  FScrollBar.Align:= alRight;
  FScrollBar.Width:= FParent.Scale96ToForm(SCROLLBAR_WIDTH_DEFAULT);
  FScrollBar.Visible:= False;

  FPanel:= TPanel.Create(nil);
  FPanel.OnMouseWheel:= @MouseWheel;
  FPanel.Parent:= FParent;
  FPanel.Color:= clWindow;
  FPanel.BevelOuter:= bvNone;
  FPanel.BevelInner:= bvNone;
  FPanel.BorderStyle:= bsNone;
  FPanel.Left:= 0;
  FPanel.Top:= 0;
  FPanel.Width:= FParent.ClientWidth;
  FPanel.Height:= FParent.ClientHeight;
end;

destructor TVSTParamList.Destroy;
var
  i: Integer;
begin
  for i:= 0 to High(FItems) do
    FreeAndNil(FItems[i]);
  FreeAndNil(FPanel);
  FreeAndNil(FScrollBar);
  FreeAndNil(FFont);

  inherited Destroy;
end;

procedure TVSTParamList.Show;
var
  i: Integer;
begin
  FHeight:= 0;
  if Length(FTrees)=0 then Exit;
  for i:= 0 to High(FItems) do
  begin
    FTrees[i].Top:= FHeight;
    FTrees[i].Height:= FItems[i].TotalHeight + 6*Ord(Screen.PixelsPerInch>96);
    FHeight:= FHeight + FTrees[i].Height + FSpace;
  end;
  FPanel.Height:= FHeight;
  //ResizeControls;
end;

procedure TVSTParamList.AddCustomList(const AListType: TVSTListType; const AName: String);
var
  VT: TVirtualStringTree;
  N: Integer;
begin
  N:= Length(FTrees);
  SetLength(FTrees, N+1);
  VT:= TVirtualStringTree.Create(FPanel);
  VT.Parent:= FPanel;
  VT.Left:= 0;
  VT.OnMouseWheel:= @MouseWheel;
  FTrees[N]:= VT;

  SetLength(FTypes, N+1);
  FTypes[N]:= AListType;
  VAppend(FNames, AName);
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
  List:= TVSTStringList.Create(FTrees[N], ACaption, AOnSelect);
  List.Update(AItems, ASelectedIndex);
  FItems[N]:= List;
end;

procedure TVSTParamList.AddCheckList(const AName, ACaption: String;
                            const AItems: TStrVector;
                            const AOnSelect: TVSTEvent;
                            const ACheckedCount: Integer = -1);
var
  N: Integer;
  List: TVSTCheckList;
begin
  AddCustomList(ltCheck, AName);

  N:= Length(FItems);
  SetLength(FItems, N+1);
  List:= TVSTCheckList.Create(FTrees[N], ACaption, AItems, AOnSelect, ACheckedCount);
  FItems[N]:= List;
end;

end.

