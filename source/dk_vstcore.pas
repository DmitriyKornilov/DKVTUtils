unit DK_VSTCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, VirtualTrees, fpstypes,

  DK_Vector, DK_Matrix, DK_Color, DK_VSTTypes;

type

  { TVSTCoreTable }

  TVSTCoreTable = class(TObject)
  protected
    FTree: TVirtualStringTree;

    FOnSelect: TVSTEvent;

    FAutosizeColumnIndex: Integer; //LAST_COLUMN_INDEX_FOR_AUTOSIZE - last clolumn, -1 - none
    FFixedColumnsCount: Integer;

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
    procedure SetGridLinesVisible(AValue: Boolean);
    procedure SetGridLinesColor(AValue: TColor);
    procedure SetHeaderBGColor(AValue: TColor);
    procedure SetHeaderFont(AValue: TFont);
    procedure SetSelectedBGColor(AValue: TColor);
    procedure SetSelectedFont(AValue: TFont);
    procedure SetValuesBGColor(AValue: TColor);
    procedure SetValuesFont(AValue: TFont);
    procedure SetFixedColumnsCount(AValue: Integer);
    procedure SetSpan(AValue: Boolean);
    procedure SetRowHeight(const AHeight: Integer);
    procedure SetHeaderHeight(const AHeight: Integer);
    procedure SetDefaultHeights(const AHeaderHeight, ARowHeight: Integer);

    procedure SetVisible(AValue: Boolean);
    function GetVisible: Boolean;

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

    procedure SetSingleFont(const AFont: TFont); virtual;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property ValuesFont: TFont read FValuesFont write SetValuesFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property FixedColumnsCount: Integer read FFixedColumnsCount write SetFixedColumnsCount;

    property Span: Boolean read FSpan write SetSpan;
    property Tree: TVirtualStringTree read FTree;

    property OnSelect: TVSTEvent read FOnSelect write FOnSelect;
  end;

  { TVSTCustomSimpleTable }

  TVSTCustomSimpleTable = class (TVSTCoreTable)
  protected
    FDataValues: TStrMatrix;
    FAutoHeight: Boolean;
    FMaxAutoHeightRowCount: Integer;
    function GetTotalHeight: Integer;
    function GetCount: Integer;
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    procedure Draw; virtual;
    procedure ValuesClear; override;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight;
    property TotalHeight: Integer read GetTotalHeight;
    property MaxAutoHeightRowCount: Integer read FMaxAutoHeightRowCount write FMaxAutoHeightRowCount;
    property Count: Integer read GetCount;
  end;

implementation

{ TVSTCoreTable }

procedure TVSTCoreTable.SetHeaderVisible(AValue: Boolean);
begin
  if FHeaderVisible=AValue then Exit;
  FHeaderVisible:= AValue;
  if FHeaderVisible then
    FTree.Header.Options:= FTree.Header.Options + [hoVisible]
  else
    FTree.Header.Options:= FTree.Header.Options - [hoVisible];
end;

procedure TVSTCoreTable.SetGridLinesColor(AValue: TColor);
begin
  if FGridLinesColor=AValue then Exit;
  FGridLinesColor:= AValue;
  FTree.Colors.GridLineColor:= FGridLinesColor;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetGridLinesVisible(AValue: Boolean);
begin
  if FGridLinesVisible=AValue then Exit;
  FGridLinesVisible:= AValue;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetFixedColumnsCount(AValue: Integer);
var
  i: Integer;
begin
  if FFixedColumnsCount=AValue then Exit;
  if (FFixedColumnsCount<0) or (FFixedColumnsCount>High(FHeaderCaptions)) then Exit;
  FFixedColumnsCount:= AValue;
  for i:= 0 to FFixedColumnsCount-1 do
    FTree.Header.Columns[i].Options:= FTree.Header.Columns[i].Options + [coFixed];
  for i:= FFixedColumnsCount to High(FHeaderCaptions) do
    FTree.Header.Columns[i].Options:= FTree.Header.Columns[i].Options - [coFixed];
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetSpan(AValue: Boolean);
begin
  if FSpan=AValue then Exit;
  FSpan:=AValue;
  if FSpan then
    FTree.TreeOptions.AutoOptions:= FTree.TreeOptions.AutoOptions + [toAutoSpanColumns]
  else
    FTree.TreeOptions.AutoOptions:= FTree.TreeOptions.AutoOptions - [toAutoSpanColumns];
  FTree.Refresh;
end;

function TVSTCoreTable.GetVisible: Boolean;
begin
  Result:= FTree.Visible;
end;

procedure TVSTCoreTable.AfterColumnWidthTracking(Sender: TVTHeader; Column: TColumnIndex);
begin
  FColumnWidths[Column]:= Sender.Columns[Column].Width;
end;

procedure TVSTCoreTable.SetVisible(AValue: Boolean);
begin
  FTree.Visible:= AValue;
end;

function TVSTCoreTable.CellRectangle(Column: TColumnIndex; ACellRect: TRect): TRect;
begin
  Result:= ACellRect;
end;

function TVSTCoreTable.NodeFromIndex(const AIndex: Integer): PVirtualNode;
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

function TVSTCoreTable.NodeFromIndex(const AIndex1, AIndex2: Integer): PVirtualNode;
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

procedure TVSTCoreTable.SetCanSelect(AValue: Boolean);
begin
  if FCanSelect=AValue then Exit;
  FCanSelect:=AValue;
end;

procedure TVSTCoreTable.HeaderClear;
begin
  FHeaderCaptions:= nil;
  FColumnWidths:= nil;
  FColumnValuesBGColors:= nil;
  FColumnHeaderBGColors:= nil;
  FTree.Header.Columns.Clear;
end;

procedure TVSTCoreTable.SetColumnWidths;
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
var
  LineColor, BGColor: TColor;
begin
  PaintInfo.TargetCanvas.Font.Assign(FHeaderFont);
  HeaderCellColors(PaintInfo, LineColor, BGColor);
  VSTHeaderDraw(LineColor, BGColor, PaintInfo, Elements);
end;

procedure TVSTCoreTable.BeforeCellPaint(Sender: TBaseVirtualTree;
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

procedure TVSTCoreTable.DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  CellFont(Node, Column);
  TargetCanvas.Font.Assign(FCellFont);
end;

function TVSTCoreTable.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= Assigned(Node) and (Column>=0) and (Column<=High(FHeaderCaptions));
end;

procedure TVSTCoreTable.HeaderCellColors(const PaintInfo: THeaderPaintInfo;
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

function TVSTCoreTable.CellGridColor(Node: PVirtualNode; Column: TColumnIndex; ABGColor: TColor): TColor;
begin
  if FGridLinesVisible then
    Result:= FGridLinesColor
  else
    Result:= ABGColor;
end;

function TVSTCoreTable.CellBGColor(Node: PVirtualNode; Column: TColumnIndex): TColor;
begin
  if IsCellSelected(Node, Column) then
    Result:= FSelectedBGColor
  else if FColumnValuesBGColors[Column]<>clNone then
    Result:= FColumnValuesBGColors[Column]
  else
    Result:= FValuesBGColor;
end;

procedure TVSTCoreTable.CellFont(Node: PVirtualNode; Column: TColumnIndex);
begin
  if IsCellSelected(Node, Column) then
    FCellFont.Assign(FSelectedFont)
  else
    FCellFont.Assign(FValuesFont);
end;

function TVSTCoreTable.IsColIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= (AIndex>=0) and (AIndex<=High(FHeaderCaptions));
end;

procedure TVSTCoreTable.SetDefaultHeights(const AHeaderHeight, ARowHeight: Integer);
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

constructor TVSTCoreTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  FTree:= ATree;

  Clear;
  FFixedColumnsCount:= 0;

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

destructor TVSTCoreTable.Destroy;
begin
  FreeAndNil(FHeaderFont);
  FreeAndNil(FValuesFont);
  FreeAndNil(FSelectedFont);
  FreeAndNil(FCellFont);
  inherited Destroy;
end;

procedure TVSTCoreTable.ValuesClear;
begin
  FTree.Clear;
end;

procedure TVSTCoreTable.Clear;
begin
  ValuesClear;
  HeaderClear;
end;

procedure TVSTCoreTable.Refresh;
begin
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetZoom(const APercents: Integer);
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

procedure TVSTCoreTable.SetFocus;
begin
  if Assigned(FTree) then FTree.SetFocus;
end;

procedure TVSTCoreTable.AddColumn(const ACaption: String;
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

procedure TVSTCoreTable.SetColumnValuesBGColor(const AColIndex: Integer; const ABGColor: TColor);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FColumnValuesBGColors[AColIndex]:= ABGColor;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetColumnValuesBGColor(const ACaption: String; const ABGColor: TColor);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColIndex>=0 then
    SetColumnValuesBGColor(ColIndex, ABGColor);
end;

procedure TVSTCoreTable.SetColumnHeaderBGColor(const AColIndex: Integer; const ABGColor: TColor);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FColumnHeaderBGColors[AColIndex]:= ABGColor;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetColumnHeaderBGColor(const ACaption: String;  const ABGColor: TColor);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColIndex>=0 then
    SetColumnHeaderBGColor(ColIndex, ABGColor);
end;

procedure TVSTCoreTable.SetRowHeight(const AHeight: Integer);
begin
  FRowHeight:= Tree.Scale96ToForm(AHeight);
  FTree.DefaultNodeHeight:= FRowHeight;
  VSTNodeHeights(FTree, FRowHeight);
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetHeaderHeight(const AHeight: Integer);
begin
  FHeaderHeight:= Tree.Scale96ToForm(AHeight);
  FTree.Header.DefaultHeight:= FHeaderHeight;
  FTree.Header.Height:= FHeaderHeight;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetAllHeight(const AHeight: Integer);
begin
  SetRowHeight(AHeight);
  SetHeaderHeight(AHeight);
end;

function TVSTCoreTable.LastColumnIndex: Integer;
begin
  Result:= High(FHeaderCaptions);
end;

procedure TVSTCoreTable.AutosizeColumnEnable(const ACaption: String);
begin
  AutosizeColumnEnable(VIndexOf(FHeaderCaptions, ACaption));
end;

procedure TVSTCoreTable.AutosizeColumnEnable(const AColIndex: Integer);
begin
  if AColIndex<0 then
    if AColIndex<>LAST_COLUMN_INDEX_FOR_AUTOSIZE then
      Exit;
  FAutosizeColumnIndex:= AColIndex;
  FTree.Header.Options:= FTree.Header.Options + [hoAutoResize];
  SetColumnWidths;
end;

procedure TVSTCoreTable.AutosizeColumnEnableLast;
begin
  AutosizeColumnEnable(LAST_COLUMN_INDEX_FOR_AUTOSIZE);
end;

procedure TVSTCoreTable.AutosizeColumnDisable;
begin
  FAutosizeColumnIndex:= -1;
  FTree.Header.Options:= FTree.Header.Options - [hoAutoResize];
  SetColumnWidths;
end;

procedure TVSTCoreTable.SetColumnWidth(const AColIndex, AWidth: Integer);
var
  W: Integer;
begin
  if not IsColIndexCorrect(AColIndex) then Exit;

  W:= Tree.Scale96ToScreen(AWidth);
  FColumnWidths[AColIndex]:= W;
  Tree.Header.Columns[AColIndex].Width:= W;
end;

procedure TVSTCoreTable.SetColumnWidth(const ACaption: String; const AWidth: Integer);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnWidth(ColIndex, AWidth);
end;

function TVSTCoreTable.GetColumnWidth(const AColIndex: Integer): Integer;
begin
  Result:= 0;
  if not IsColIndexCorrect(AColIndex) then Exit;
  Result:= Tree.ScaleScreenTo96(Tree.Header.Columns[AColIndex].Width);
end;

function TVSTCoreTable.GetColumnWidth(const ACaption: String): Integer;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  Result:= GetColumnWidth(ColIndex);
end;

procedure TVSTCoreTable.RenameColumn(const AColIndex: Integer; const ANewName: String);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FHeaderCaptions[AColIndex]:= ANewName;
  FTree.Header.Columns[AColIndex].Text:= FHeaderCaptions[AColIndex];
end;

procedure TVSTCoreTable.RenameColumn(const AOldName, ANewName: String);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, AOldName);
  RenameColumn(ColIndex, ANewName);
end;

procedure TVSTCoreTable.SetSingleFont(const AFont: TFont);
begin
  HeaderFont:= AFont;
  ValuesFont:= AFont;
  SelectedFont:= AFont;
end;

{ TVSTCustomSimpleTable }

function TVSTCustomSimpleTable.GetTotalHeight: Integer;
var
  RowCount: Integer;
begin
  RowCount:= Count;
  if RowCount=0 then
    RowCount:= 1;
  if (MaxAutoHeightRowCount>0) and (RowCount>MaxAutoHeightRowCount) then
    RowCount:= MaxAutoHeightRowCount;
  Result:= FHeaderHeight*Ord(FHeaderVisible) + RowCount*FRowHeight;
end;

function TVSTCustomSimpleTable.GetCount: Integer;
begin
  Result:= MMaxLength(FDataValues);
end;

constructor TVSTCustomSimpleTable.Create(const ATree: TVirtualStringTree;
  const AHeaderHeight: Integer; const ARowHeight: Integer);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FAutoHeight:= False;
  FMaxAutoHeightRowCount:= 0;
  FTree.Indent:= 0;
end;

procedure TVSTCustomSimpleTable.Draw;
begin
  SetColumnWidths;

  if AutoHeight and
    (FTree.Align<>alLeft)   and
    (FTree.Align<>alRight)  and
    (FTree.Align<>alClient) then
      FTree.Height:= TotalHeight;
end;

procedure TVSTCustomSimpleTable.ValuesClear;
var
  i: Integer;
begin
  for i:=0 to High(FDataValues) do
    FDataValues[i]:= nil;
  inherited ValuesClear;
end;

end.

