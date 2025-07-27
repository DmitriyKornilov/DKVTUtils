unit DK_VSTCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, VirtualTrees, fpstypes,

  DK_Vector, DK_Matrix, DK_Color, DK_VSTTypes;

type

  { TVSTCore }

  TVSTCore = class(TObject)
  protected
    FTree: TVirtualStringTree;

    FOnSelect: TVSTEvent;

    FAutosizeColumnIndex: Integer; //LAST_COLUMN_INDEX_FOR_AUTOSIZE - last clolumn, -1 - none
    FFixedColCount: Integer;

    FGridLinesColor: TColor;
    FGridLinesVisible: Boolean;

    FHeaderVisible: Boolean;

    FCanSelect: Boolean;
    FCanUnselect: Boolean;
    FSpan: Boolean;

    FValuesBGColor: TColor;
    FHeaderBGColor: TColor;
    FSelectedBGColor: TColor;
    FColumnValuesBGColors: TColorVector;
    FColumnHeaderBGColors: TColorVector;

    FHeaderCaptions: TStrVector;
    FColumnWidths: TIntVector;
    FColumnVisibles: TBoolVector;

    FHeaderFont: TFont;
    FValuesFont: TFont;
    FSelectedFont: TFont;
    FCellFont: TFont;

    FHeaderHeight: Integer;
    FRowHeight: Integer;

    function NodeFromIndex(const AIndex: Integer): PVirtualNode;
    function NodeFromIndex(const AIndex1, AIndex2: Integer): PVirtualNode;

    procedure HeaderClear; virtual;

    procedure SetCanSelect(AValue: Boolean); virtual;
    procedure SetHeaderVisible(AValue: Boolean);
    procedure SetColumnWidths;
    procedure SetColumnVisibles(AValue: TBoolVector);
    procedure SetGridLinesVisible(AValue: Boolean);
    procedure SetGridLinesColor(AValue: TColor);
    procedure SetHeaderBGColor(AValue: TColor);
    procedure SetHeaderFont(AValue: TFont);
    procedure SetSelectedBGColor(AValue: TColor);
    procedure SetSelectedFont(AValue: TFont);
    procedure SetValuesBGColor(AValue: TColor);
    procedure SetValuesFont(AValue: TFont);
    procedure SetFixedColCount(AValue: Integer);
    procedure SetSpan(AValue: Boolean);
    procedure SetRowHeight(const AHeight: Integer);
    procedure SetHeaderHeight(const AHeight: Integer);
    procedure SetDefaultHeights(const AHeaderHeight, ARowHeight: Integer);

    procedure SetVisible(AValue: Boolean);
    function GetVisible: Boolean;
    function GetColCount: Integer;

    procedure AfterColumnWidthTracking(Sender: TVTHeader; Column: TColumnIndex);
    procedure HeaderDrawQueryElements(Sender: TVTHeader;
                            var {%H-}PaintInfo: THeaderPaintInfo;
                            var Elements: THeaderPaintElements);
    procedure AdvancedHeaderDraw(Sender: TVTHeader;
                            var PaintInfo: THeaderPaintInfo;
                            const Elements: THeaderPaintElements);
    procedure BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                              Node: PVirtualNode; Column: TColumnIndex;
                              CellPaintMode: TVTCellPaintMode; CellRect: TRect;
                              var {%H-}ContentRect: TRect); virtual;
    procedure DrawText(Sender: TBaseVirtualTree;
                       TargetCanvas: TCanvas; Node: PVirtualNode;
                       {%H-}Column: TColumnIndex;
                       const {%H-}CellText: String; const {%H-}CellRect: TRect;
                       var {%H-}DefaultDraw: Boolean);

    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual;

    procedure HeaderCellColors(const PaintInfo: THeaderPaintInfo;
                              out ALineColor, ABGColor: TColor);

    function CellGridColor({%H-}Node: PVirtualNode; {%H-}Column: TColumnIndex; ABGColor: TColor): TColor; virtual;
    function CellBGColor(Node: PVirtualNode; {%H-}Column: TColumnIndex): TColor; virtual;
    procedure CellFont(Node: PVirtualNode; {%H-}Column: TColumnIndex); virtual;
    function CellRectangle({%H-}Column: TColumnIndex; ACellRect: TRect): TRect; virtual;

    function IsColIndexCorrect(const AIndex: Integer): Boolean;
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor  Destroy; override;

    procedure ValuesClear; virtual;
    procedure Clear;
    procedure Refresh;

    procedure SetZoom(const APercents: Integer);
    procedure SetFocus;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter); virtual;

    procedure SetColumnValuesBGColor(const AColIndex: Integer; const ABGColor: TColor);
    procedure SetColumnValuesBGColor(const ACaption: String; const ABGColor: TColor);
    procedure SetColumnHeaderBGColor(const AColIndex: Integer; const ABGColor: TColor);
    procedure SetColumnHeaderBGColor(const ACaption: String; const ABGColor: TColor);

    procedure SetAllHeight(const AHeight: Integer);
    function LastColumnIndex: Integer;

    procedure AutosizeColumnEnable(const ACaption: String);
    procedure AutosizeColumnEnable(const AColIndex: Integer);
    procedure AutosizeColumnEnableLast;
    procedure AutosizeColumnDisable;

    procedure SetColumnWidth(const AColIndex, AWidth{Scale96}: Integer);
    procedure SetColumnWidth(const ACaption: String; const AWidth{Scale96}: Integer);
    function GetColumnWidth(const AColIndex: Integer): Integer; {Scale96}
    function GetColumnWidth(const ACaption: String): Integer; {Scale96}

    procedure RenameColumn(const AColIndex: Integer; const ANewName: String);
    procedure RenameColumn(const AOldName, ANewName: String);

    property RowHeight: Integer read FRowHeight write SetRowHeight;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight;

    property GridLinesColor: TColor read FGridLinesColor write SetGridLinesColor;
    property ValuesBGColor: TColor read FValuesBGColor write SetValuesBGColor;
    property HeaderBGColor: TColor read FHeaderBGColor write SetHeaderBGColor;
    property SelectedBGColor: TColor read FSelectedBGColor write SetSelectedBGColor;

    property CanSelect: Boolean read FCanSelect write SetCanSelect;
    property CanUnselect: Boolean read FCanUnselect write FCanUnselect;

    property GridLinesVisible: Boolean read FGridLinesVisible write SetGridLinesVisible;
    property HeaderVisible: Boolean read FHeaderVisible write SetHeaderVisible;
    property Visible: Boolean read GetVisible write SetVisible;
    property ColumnVisibles: TBoolVector read FColumnVisibles write SetColumnVisibles;

    procedure SetSingleFont(const AFont: TFont); virtual;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property ValuesFont: TFont read FValuesFont write SetValuesFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property FixedColCount: Integer read FFixedColCount write SetFixedColCount;
    property ColCount: Integer read GetColCount;

    property Span: Boolean read FSpan write SetSpan;
    property Tree: TVirtualStringTree read FTree;

    property OnSelect: TVSTEvent read FOnSelect write FOnSelect;
  end;

implementation

{ TVSTCore }

procedure TVSTCore.SetHeaderVisible(AValue: Boolean);
begin
  if FHeaderVisible=AValue then Exit;
  FHeaderVisible:= AValue;
  if FHeaderVisible then
    FTree.Header.Options:= FTree.Header.Options + [hoVisible]
  else
    FTree.Header.Options:= FTree.Header.Options - [hoVisible];
end;

procedure TVSTCore.SetGridLinesColor(AValue: TColor);
begin
  if FGridLinesColor=AValue then Exit;
  FGridLinesColor:= AValue;
  FTree.Colors.GridLineColor:= FGridLinesColor;
  FTree.Refresh;
end;

procedure TVSTCore.SetGridLinesVisible(AValue: Boolean);
begin
  if FGridLinesVisible=AValue then Exit;
  FGridLinesVisible:= AValue;
  FTree.Refresh;
end;

procedure TVSTCore.SetFixedColCount(AValue: Integer);
var
  i: Integer;
begin
  if FFixedColCount=AValue then Exit;
  if (FFixedColCount<0) or (FFixedColCount>High(FHeaderCaptions)) then Exit;
  FFixedColCount:= AValue;
  for i:= 0 to FFixedColCount-1 do
    FTree.Header.Columns[i].Options:= FTree.Header.Columns[i].Options + [coFixed];
  for i:= FFixedColCount to High(FHeaderCaptions) do
    FTree.Header.Columns[i].Options:= FTree.Header.Columns[i].Options - [coFixed];
  FTree.Refresh;
end;

procedure TVSTCore.SetSpan(AValue: Boolean);
begin
  if FSpan=AValue then Exit;
  FSpan:=AValue;
  if FSpan then
    FTree.TreeOptions.AutoOptions:= FTree.TreeOptions.AutoOptions + [toAutoSpanColumns]
  else
    FTree.TreeOptions.AutoOptions:= FTree.TreeOptions.AutoOptions - [toAutoSpanColumns];
  FTree.Refresh;
end;

function TVSTCore.GetVisible: Boolean;
begin
  Result:= FTree.Visible;
end;

function TVSTCore.GetColCount: Integer;
begin
  Result:= Length(FHeaderCaptions);
end;

procedure TVSTCore.AfterColumnWidthTracking(Sender: TVTHeader; Column: TColumnIndex);
begin
  FColumnWidths[Column]:= Sender.Columns[Column].Width;
end;

procedure TVSTCore.SetVisible(AValue: Boolean);
begin
  FTree.Visible:= AValue;
end;

function TVSTCore.CellRectangle(Column: TColumnIndex; ACellRect: TRect): TRect;
begin
  Result:= ACellRect;
end;

function TVSTCore.NodeFromIndex(const AIndex: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result:= nil;
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if FTree.GetNodeLevel(Node)=0 then
    begin
      if (Node^.Index = AIndex) then
      begin
        Result:= Node;
        break;
      end;
    end;
    Node:= FTree.GetNext(Node);
  end;
end;

function TVSTCore.NodeFromIndex(const AIndex1, AIndex2: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result:= nil;
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if (FTree.GetNodeLevel(Node)=1) then
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

procedure TVSTCore.SetCanSelect(AValue: Boolean);
begin
  if FCanSelect=AValue then Exit;
  FCanSelect:=AValue;
end;

procedure TVSTCore.HeaderClear;
begin
  FHeaderCaptions:= nil;
  FColumnWidths:= nil;
  FColumnValuesBGColors:= nil;
  FColumnHeaderBGColors:= nil;
  FTree.Header.Columns.Clear;
end;

procedure TVSTCore.SetColumnWidths;
var
  i: Integer;
begin
  FTree.Header.AutoSizeIndex:= -1;
  if FAutosizeColumnIndex>=0 then
  begin
    if FAutosizeColumnIndex<=High(FHeaderCaptions) then
    FTree.Header.AutoSizeIndex:= FAutosizeColumnIndex;
  end
  else if FAutosizeColumnIndex=LAST_COLUMN_INDEX_FOR_AUTOSIZE then
    FTree.Header.AutoSizeIndex:= High(FHeaderCaptions);
  for i:= 0 to High(FHeaderCaptions) do
    FTree.Header.Columns[i].Width:= FColumnWidths[i];
end;

procedure TVSTCore.SetColumnVisibles(AValue: TBoolVector);
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

procedure TVSTCore.SetHeaderBGColor(AValue: TColor);
begin
  if FHeaderBGColor=AValue then Exit;
  FHeaderBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCore.SetHeaderFont(AValue: TFont);
begin
  FHeaderFont.Assign(AValue);
  FTree.Header.Font.Assign(FHeaderFont);
  FTree.Refresh;
end;

procedure TVSTCore.SetSelectedBGColor(AValue: TColor);
begin
  if FSelectedBGColor=AValue then Exit;
  FSelectedBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCore.SetSelectedFont(AValue: TFont);
begin
  FSelectedFont.Assign(AValue);
  FTree.Refresh;
end;

procedure TVSTCore.SetValuesBGColor(AValue: TColor);
begin
  if FValuesBGColor=AValue then Exit;
  FValuesBGColor:=AValue;
  FTree.Refresh;
end;

procedure TVSTCore.SetValuesFont(AValue: TFont);
begin
  FValuesFont.Assign(AValue);
  FTree.Font.Assign(FValuesFont);
  FTree.Refresh;
end;

procedure TVSTCore.HeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements:= [hpeBackground];
end;

procedure TVSTCore.AdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var
  LineColor, BGColor: TColor;
begin
  PaintInfo.TargetCanvas.Font.Assign(FHeaderFont);
  HeaderCellColors(PaintInfo, LineColor, BGColor);
  VSTHeaderDraw(LineColor, BGColor, PaintInfo, Elements);
end;

procedure TVSTCore.BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  BGColor, GridColor: TColor;
  NeedTopLine: Boolean;
begin
  if CellPaintMode<>cpmPaint then Exit;
  BGColor:= CellBGColor(Node, Column);
  GridColor:= CellGridColor(Node, Column, BGColor);
  CellRect:= CellRectangle(Column, CellRect);
  NeedTopLine:= (not (hoVisible in FTree.Header.Options)) and
                (Sender.GetDisplayRect(Node, Column, False).Top=0);
  VSTCellDraw(GridColor, BGColor, TargetCanvas, Column, CellRect, NeedTopLine);
end;

procedure TVSTCore.DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  CellFont(Node, Column);
  TargetCanvas.Font.Assign(FCellFont);
end;

function TVSTCore.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= Assigned(Node) and (Column>=0) and (Column<=High(FHeaderCaptions));
end;

procedure TVSTCore.HeaderCellColors(const PaintInfo: THeaderPaintInfo;
                                     out ALineColor, ABGColor: TColor);
begin
  ALineColor:= FTree.Color;
  ABGColor:= FTree.Color;

  if (not Assigned(PaintInfo.Column)) or
     (not IsColIndexCorrect(PaintInfo.Column.Index)) then Exit;

  if FColumnHeaderBGColors[PaintInfo.Column.Index]<>clNone then
    ABGColor:= FColumnHeaderBGColors[PaintInfo.Column.Index]
  else
    ABGColor:= FHeaderBGColor;

  if FGridLinesVisible then
    ALineColor:= FGridLinesColor
  else
    ALineColor:= ABGColor;
end;

function TVSTCore.CellGridColor(Node: PVirtualNode; Column: TColumnIndex; ABGColor: TColor): TColor;
begin
  if FGridLinesVisible then
    Result:= FGridLinesColor
  else
    Result:= ABGColor;
end;

function TVSTCore.CellBGColor(Node: PVirtualNode; Column: TColumnIndex): TColor;
begin
  if IsCellSelected(Node, Column) then
    Result:= FSelectedBGColor
  else if FColumnValuesBGColors[Column]<>clNone then
    Result:= FColumnValuesBGColors[Column]
  else
    Result:= FValuesBGColor;
end;

procedure TVSTCore.CellFont(Node: PVirtualNode; Column: TColumnIndex);
begin
  if IsCellSelected(Node, Column) then
    FCellFont.Assign(FSelectedFont)
  else
    FCellFont.Assign(FValuesFont);
end;

function TVSTCore.IsColIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= (AIndex>=0) and (AIndex<=High(FHeaderCaptions));
end;

procedure TVSTCore.SetDefaultHeights(const AHeaderHeight, ARowHeight: Integer);
var
  H: Integer;
begin
  H:= Tree.ScaleFormTo96(FTree.DefaultNodeHeight);
  if ARowHeight>H then
    RowHeight:= ARowHeight
  else
    RowHeight:=H;
  H:= Tree.ScaleFormTo96(FTree.Header.DefaultHeight);
  if AHeaderHeight>H then
    HeaderHeight:= AHeaderHeight
  else
    HeaderHeight:= H;
end;

constructor TVSTCore.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  FTree:= ATree;

  Clear;
  FFixedColCount:= 0;

  FTree.Header.Font.Color:= COLOR_FONT_DEFAULT;
  FTree.Font.Color:= COLOR_FONT_DEFAULT;
  FTree.ScrollBarOptions.ScrollBars:= ssBoth;
  FTree.ScrollBarOptions.AlwaysVisible:= False;

  FHeaderVisible:= True;
  FHeaderFont:= TFont.Create;
  FValuesFont:= TFont.Create;
  FSelectedFont:= TFont.Create;
  FCellFont:= TFont.Create;
  FHeaderFont.Assign(FTree.Header.Font);
  FValuesFont.Assign(FTree.Font);
  FSelectedFont.Assign(FTree.Font);
  FCellFont.Assign(FTree.Font);

  FGridLinesVisible:= True;
  FGridLinesColor:= COLOR_LINE_DEFAULT;
  FValuesBGColor:= COLOR_BG_DEFAULT;
  FHeaderBGColor:= FValuesBGColor;
  FSelectedBGColor:= DefaultSelectionBGColor;

  FCanSelect:= False;
  FCanUnselect:= True;
  Span:= False;

  FTree.HintMode:= hmTooltip;
  FTree.ShowHint:= True;

  FTree.Colors.GridLineColor:= FGridLinesColor;
  FTree.Color:= FValuesBGColor;

  SetDefaultHeights(AHeaderHeight, ARowHeight);

  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions +
                                   [toAlwaysHideSelection, toHideFocusRect];

  FTree.Header.Options:= FTree.Header.Options + [hoOwnerDraw, hoVisible];
  AutosizeColumnEnableLast;

  FTree.OnHeaderDrawQueryElements:= @HeaderDrawQueryElements;
  FTree.OnAdvancedHeaderDraw:= @AdvancedHeaderDraw;
  FTree.OnBeforeCellPaint:= @BeforeCellPaint;
  FTree.OnDrawText:= @DrawText;
  FTree.OnAfterColumnWidthTracking:= @AfterColumnWidthTracking;
end;

destructor TVSTCore.Destroy;
begin
  FreeAndNil(FHeaderFont);
  FreeAndNil(FValuesFont);
  FreeAndNil(FSelectedFont);
  FreeAndNil(FCellFont);
  inherited Destroy;
end;

procedure TVSTCore.ValuesClear;
begin
  FTree.Clear;
end;

procedure TVSTCore.Clear;
begin
  ValuesClear;
  HeaderClear;
end;

procedure TVSTCore.Refresh;
begin
  FTree.Refresh;
end;

procedure TVSTCore.SetZoom(const APercents: Integer);
var
  ZoomFactor: Double;
  i: Integer;
begin
  ZoomFactor:= APercents/100;
  FTree.BeginUpdate;
  try
    FTree.Font.Height:= Round(ValuesFont.Height * ZoomFactor);
    FTree.Header.Font.Height := Round(HeaderFont.Height * ZoomFactor);
    //FTree.Font.Size:= Round(ValuesFont.Size * ZoomFactor);
    //FTree.Header.Font.Size := Round(HeaderFont.Size * ZoomFactor);
    FTree.Header.Height := Round(FHeaderHeight * ZoomFactor);
    for i := 0 to High(FColumnWidths) do
      FTree.Header.Columns.Items[i].Width:= Round(FColumnWidths[i] * ZoomFactor);
    VSTNodeHeights(FTree, Round(FRowHeight * ZoomFactor));
  finally
    FTree.EndUpdate;
  end;
end;

procedure TVSTCore.SetFocus;
begin
  if Assigned(FTree) then FTree.SetFocus;
end;

procedure TVSTCore.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
var
  C: TVirtualTreeColumn;
  W: Integer;
begin
  W:= Tree.Scale96ToScreen(AWidth);
  VAppend(FHeaderCaptions, ACaption);
  VAppend(FColumnWidths, W);
  VAppend(FColumnValuesBGColors, clNone);
  VAppend(FColumnHeaderBGColors, clNone);
  C:= FTree.Header.Columns.Add;
  C.Text:= ACaption;
  C.CaptionAlignment:= ACaptionAlignment;
  C.Margin:= 3;
  C.Spacing:= 0;
  C.Width:= W;
end;

procedure TVSTCore.SetColumnValuesBGColor(const AColIndex: Integer; const ABGColor: TColor);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FColumnValuesBGColors[AColIndex]:= ABGColor;
  FTree.Refresh;
end;

procedure TVSTCore.SetColumnValuesBGColor(const ACaption: String; const ABGColor: TColor);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColIndex>=0 then
    SetColumnValuesBGColor(ColIndex, ABGColor);
end;

procedure TVSTCore.SetColumnHeaderBGColor(const AColIndex: Integer; const ABGColor: TColor);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FColumnHeaderBGColors[AColIndex]:= ABGColor;
  FTree.Refresh;
end;

procedure TVSTCore.SetColumnHeaderBGColor(const ACaption: String;  const ABGColor: TColor);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColIndex>=0 then
    SetColumnHeaderBGColor(ColIndex, ABGColor);
end;

procedure TVSTCore.SetRowHeight(const AHeight: Integer);
begin
  FRowHeight:= Tree.Scale96ToForm(AHeight);
  FTree.DefaultNodeHeight:= FRowHeight;
  VSTNodeHeights(FTree, FRowHeight);
  FTree.Refresh;
end;

procedure TVSTCore.SetHeaderHeight(const AHeight: Integer);
begin
  FHeaderHeight:= Tree.Scale96ToForm(AHeight);
  FTree.Header.DefaultHeight:= FHeaderHeight;
  FTree.Header.Height:= FHeaderHeight;
  FTree.Refresh;
end;

procedure TVSTCore.SetAllHeight(const AHeight: Integer);
begin
  SetRowHeight(AHeight);
  SetHeaderHeight(AHeight);
end;

function TVSTCore.LastColumnIndex: Integer;
begin
  Result:= High(FHeaderCaptions);
end;

procedure TVSTCore.AutosizeColumnEnable(const ACaption: String);
begin
  AutosizeColumnEnable(VIndexOf(FHeaderCaptions, ACaption));
end;

procedure TVSTCore.AutosizeColumnEnable(const AColIndex: Integer);
begin
  if AColIndex<0 then
    if AColIndex<>LAST_COLUMN_INDEX_FOR_AUTOSIZE then
      Exit;
  FAutosizeColumnIndex:= AColIndex;
  FTree.Header.Options:= FTree.Header.Options + [hoAutoResize];
  SetColumnWidths;
end;

procedure TVSTCore.AutosizeColumnEnableLast;
begin
  AutosizeColumnEnable(LAST_COLUMN_INDEX_FOR_AUTOSIZE);
end;

procedure TVSTCore.AutosizeColumnDisable;
begin
  FAutosizeColumnIndex:= -1;
  FTree.Header.Options:= FTree.Header.Options - [hoAutoResize];
  SetColumnWidths;
end;

procedure TVSTCore.SetColumnWidth(const AColIndex, AWidth: Integer);
var
  W: Integer;
begin
  if not IsColIndexCorrect(AColIndex) then Exit;

  W:= Tree.Scale96ToScreen(AWidth);
  FColumnWidths[AColIndex]:= W;
  Tree.Header.Columns[AColIndex].Width:= W;
end;

procedure TVSTCore.SetColumnWidth(const ACaption: String; const AWidth: Integer);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnWidth(ColIndex, AWidth);
end;

function TVSTCore.GetColumnWidth(const AColIndex: Integer): Integer;
begin
  Result:= 0;
  if not IsColIndexCorrect(AColIndex) then Exit;
  Result:= Tree.ScaleScreenTo96(Tree.Header.Columns[AColIndex].Width);
end;

function TVSTCore.GetColumnWidth(const ACaption: String): Integer;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  Result:= GetColumnWidth(ColIndex);
end;

procedure TVSTCore.RenameColumn(const AColIndex: Integer; const ANewName: String);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FHeaderCaptions[AColIndex]:= ANewName;
  FTree.Header.Columns[AColIndex].Text:= FHeaderCaptions[AColIndex];
end;

procedure TVSTCore.RenameColumn(const AOldName, ANewName: String);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, AOldName);
  RenameColumn(ColIndex, ANewName);
end;

procedure TVSTCore.SetSingleFont(const AFont: TFont);
begin
  HeaderFont:= AFont;
  ValuesFont:= AFont;
  SelectedFont:= AFont;
end;

end.

