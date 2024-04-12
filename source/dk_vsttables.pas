unit DK_VSTTables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, VirtualTrees, StdCtrls, Spin,
  DateTimePicker, LMessages, LCLIntf, Forms, GraphUtil,
  BCComboBox, BCTypes, BCButton, Dialogs, fpstypes,

  DK_VSTUtils, DK_Vector, DK_Matrix, DK_StrUtils, DK_Const, DK_Color, DK_PPI,
  DK_SheetExporter, DK_SheetWriter;

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
  TVSTCheckEvent = procedure(const ARowIndex: Integer; const AChecked: Boolean) of object;
  TVSTCellCheckEvent = procedure(const ARowIndex, AColIndex: Integer; const AChecked: Boolean) of object;
  TVSTEdititingDoneEvent = procedure(const ARowIndex, AColIndex: Integer;
                                     const ANewText: String;
                                     const AColumnType: TVSTColumnType;
                                     const ASaveChanges: Boolean) of object;
  TVSTEdititingBeginEvent = procedure of object;

  { TVSTCoreTable }

  TVSTCoreTable = class(TObject)
  protected
    FTree: TVirtualStringTree;

    FOnSelect: TVSTEvent;

    FDesignTimePPI: Integer;

    FAutosizeColumnIndex: Integer; //LAST_COLUMN_INDEX_FOR_AUTOSIZE - last clolumn, -1 - none
    FFixedColumnsCount: Integer;

    FGridLinesColor: TColor;
    FGridLinesVisible: Boolean;

    FHeaderVisible: Boolean;

    FCanSelect: Boolean;
    FCanUnselect: Boolean;

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

    function NodeFromIndex(const AIndex: Integer): PVirtualNode;
    function NodeFromIndex(const AIndex1, AIndex2: Integer): PVirtualNode;

    procedure SetCanSelect(AValue: Boolean); virtual;
    procedure HeaderClear; virtual;
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
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    procedure HeaderDrawQueryElements(Sender: TVTHeader;
                            var {%H-}PaintInfo: THeaderPaintInfo;
                            var Elements: THeaderPaintElements);
    procedure AdvancedHeaderDraw(Sender: TVTHeader;
                            var PaintInfo: THeaderPaintInfo;
                            const Elements: THeaderPaintElements);
    procedure BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                              Node: PVirtualNode; Column: TColumnIndex;
                              CellPaintMode: TVTCellPaintMode; CellRect: TRect;
                              var {%H-}ContentRect: TRect);
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

    procedure SetDesignTimePPI;
    procedure SetDefaultHeights(const AHeaderHeight, ARowHeight: Integer);

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

    procedure SetRowHeight(const AHeight: Integer);
    procedure SetHeaderHeight(const AHeight: Integer);
    procedure SetAllHeight(const AHeight: Integer);

    procedure AutosizeColumnEnable(const ACaption: String);
    procedure AutosizeColumnEnable(const AColIndex: Integer);
    procedure AutosizeColumnEnableLast;
    procedure AutosizeColumnDisable;

    procedure RenameColumn(const AColIndex: Integer; const ANewName: String);
    procedure RenameColumn(const AOldName, ANewName: String);

    property GridLinesColor: TColor read FGridLinesColor write SetGridLinesColor;
    property ValuesBGColor: TColor read FValuesBGColor write SetValuesBGColor;
    property HeaderBGColor: TColor read FHeaderBGColor write SetHeaderBGColor;
    property SelectedBGColor: TColor read FSelectedBGColor write SetSelectedBGColor;

    property CanSelect: Boolean read FCanSelect write SetCanSelect;
    property CanUnselect: Boolean read FCanUnselect write FCanUnselect;

    property GridLinesVisible: Boolean read FGridLinesVisible write SetGridLinesVisible;
    property HeaderVisible: Boolean read FHeaderVisible write SetHeaderVisible;
    property Visible: Boolean read GetVisible write SetVisible;

    procedure SetSingleFont(const AFont: TFont);
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property ValuesFont: TFont read FValuesFont write SetValuesFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property FixedColumnsCount: Integer read FFixedColumnsCount write SetFixedColumnsCount;
    property DesignTimePPI: Integer read FDesignTimePPI;

    property OnSelect: TVSTEvent read FOnSelect write FOnSelect;
  end;

  { TVSTCustomSimpleTable }

  TVSTCustomSimpleTable = class (TVSTCoreTable)
  protected
    FDataValues: TStrMatrix;
    FAutoHeight: Boolean;
    FMaxAutoHeightRowCount: Integer;
    function GetTotalHeight: Integer;
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    procedure Draw; virtual;
    procedure ValuesClear; override;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight;
    property TotalHeight: Integer read GetTotalHeight;
    property MaxAutoHeightRowCount: Integer read FMaxAutoHeightRowCount write FMaxAutoHeightRowCount;
  end;

  { TVSTEdit }

  TVSTEdit = class (TVSTCustomSimpleTable)
  protected
    FPicks: TStrMatrix;
    FKeys: TIntMatrix;
    FCellTextBeforeEditing: String;
    FColumnTypes: TVSTColumnTypes;//array of TVSTColumnType;
    FColumnFormatStrings: TStrVector;
    FDecimalPlaces: TIntVector;
    FSelectedRowIndex, FSelectedColIndex: Integer;
    FTitleColumnIndex: Integer;
    FShowZeros: Boolean;
    FUnselectOnExit: Boolean;
    FIsEditing: Boolean;
    FIsOneRowEditing: Boolean;
    FIsBeginEditOnKeyPress: Boolean;
    FEditor: TWinControl;
    FOnEdititingDone: TVSTEdititingDoneEvent;
    FOnEdititingBegin: TVSTEdititingBeginEvent;
    FColumnRowTitlesFont: TFont;
    //FColumnRowTitlesBGColor: TColor;
    FColorColumnBorderColor: TColor;
    FColorColumnCellMargin: Integer;

    procedure SelectCell(Node: PVirtualNode; Column: TColumnIndex; const ASaveChanges: Boolean = True);
    procedure UnselectCell(const ASaveChanges: Boolean = True);

    procedure HeaderClear; override;

    function IsColumnValuesExists(const AColIndex: Integer): Boolean;

    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    function GetIsSelected: Boolean;
    function GetSelectedText: String;
    procedure SetSelectedText(AValue: String);
    function SelectedCellRect: TRect;

    procedure SetUnselectOnExit(AValue: Boolean);
    procedure SetShowZeros(AValue: Boolean);

    function IsEditingColIndexCorrect(const AIndex: Integer): Boolean;
    function IsRowIndexCorrect(const AIndex: Integer): Boolean;

    function GetColumnRowTitlesBGColor: TColor;
    procedure SetColumnRowTitlesBGColor(AValue: TColor);
    procedure SetColumnRowTitlesFont(AValue: TFont);
    procedure SetColumnRowTitlesVisible(AValue: Boolean);

    procedure SetNewColumnSettings(const AColumnType: TVSTColumnType;
                                   const AFormatString: String;
                                   const ADecimalPlaces: Integer = 0);
    procedure AddValuesColumn(const AColumnType: TVSTColumnType;
                        const ACaption, AFormatString: String;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter;
                        const AKeys: TIntVector = nil;
                        const APicks: TStrVector = nil;
                        const ADecimalPlaces: Integer = 0);

    function GetIsRowTitlesColumnExists: Boolean;

    function GetRowValues(const ARowIndex: Integer): TStrVector;
    procedure SetRowValues(const ARowIndex: Integer; const ARowValues: TStrVector);

    function CellRectangle(Column: TColumnIndex; ACellRect: TRect): TRect; override;
    function CellGridColor(Node: PVirtualNode; {%H-}Column: TColumnIndex; ABGColor: TColor): TColor; override;
    function CellBGColor(Node: PVirtualNode; {%H-}Column: TColumnIndex): TColor; override;
    procedure CellFont(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DeleteSelectedCellText;

    procedure MoveSelectionVertical(const ADirection {1 down, -1 up}: Integer);
    procedure MoveSelectionHorizontal(const ADirection {1 right, -1 left}: Integer);

    procedure BeginEdit;
    procedure EndEdit(const ASaveChanges: Boolean = True);

    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);
    procedure NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure KeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure EditorKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure EditorExit;
    procedure TreeExit(Sender: TObject);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor  Destroy; override;

    procedure ValuesClear; override;
    procedure Draw; override;

    procedure AutosizeColumnRowTitlesEnable;

    procedure AddColumnRowTitles(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter);
    procedure SetColumnRowTitles(const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnInteger(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnDouble(const ACaption: String;
                        const ADecimalPlaces: Integer = 2;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnString(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnDate(const ACaption, AFormatString: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnTime(const ACaption, AFormatString: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnKeyPick(const ACaption: String;
                        const AKeys: TIntVector; const APicks: TStrVector;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure AddColumnColor(const ACaption: String; const AWidth: Integer = 100;
                             const ACaptionAlignment: TAlignment = taCenter);

    procedure SetColumnInteger(const ACaption: String; const AValues: TIntVector);
    procedure SetColumnDouble(const ACaption: String; const AValues: TDblVector);
    procedure SetColumnString(const ACaption: String; const AValues: TStrVector);
    procedure SetColumnDate(const ACaption: String; const AValues: TDateVector);
    procedure SetColumnTime(const ACaption: String; const AValues: TTimeVector);
    procedure SetColumnColor(const ACaption: String; const AValues: TColorVector);

    procedure SetColumnInteger(const AColIndex: Integer; const AValues: TIntVector);
    procedure SetColumnDouble(const AColIndex: Integer; const AValues: TDblVector);
    procedure SetColumnString(const AColIndex: Integer; const AValues: TStrVector);
    procedure SetColumnDate(const AColIndex: Integer; const AValues: TDateVector);
    procedure SetColumnTime(const AColIndex: Integer; const AValues: TTimeVector);
    procedure SetColumnColor(const AColIndex: Integer; const AValues: TColorVector);

    procedure UnSelect(const ASaveChanges: Boolean = True);
    procedure Select(const ARowIndex, AColIndex: Integer);
    procedure Select(const ARowIndex: Integer; const AColumnCaption: String);
    procedure Select(const ARowTitle, AColumnCaption: String);

    procedure Show(const ARowIndex: Integer);

    function ColumnAsInteger(out AValues: TIntVector; const AColIndex: Integer;
                             const ADefaultValue: Integer = 0): Boolean;
    function ColumnAsString(out AValues: TStrVector; const AColIndex: Integer;
                             const ADefaultValue: String = ''): Boolean;
    function ColumnAsDate(out AValues: TDblVector; const AColIndex: Integer;
                          const ADefaultValue: TDate = 0): Boolean;
    function ColumnAsTime(out AValues: TDblVector; const AColIndex: Integer;
                          const ADefaultValue: TTime = 0): Boolean;
    function ColumnAsColor(out AValues: TColorVector; const AColIndex: Integer;
                          const ADefaultValue: TColor = clBlack): Boolean;

    function ColumnAsInteger(out AValues: TIntVector; const ACaption: String;
                             const ADefaultValue: Integer = 0): Boolean;
    function ColumnAsString(out AValues: TStrVector; const ACaption: String;
                             const ADefaultValue: String = ''): Boolean;
    function ColumnAsDate(out AValues: TDblVector; const ACaption: String;
                          const ADefaultValue: TDate = 0): Boolean;
    function ColumnAsTime(out AValues: TDblVector; const ACaption: String;
                          const ADefaultValue: TTime = 0): Boolean;
    function ColumnAsColor(out AValues: TColorVector; const ACaption: String;
                          const ADefaultValue: TColor = clBlack): Boolean;

    procedure RowInsert(const ARowIndex: Integer; const AValues: TStrVector = nil);
    procedure RowDelete(const ARowIndex: Integer);
    property RowValues[ARowIndex: Integer]: TStrVector read GetRowValues write SetRowValues;

    procedure SetColumnRowTitlesHeaderBGColor(const ABGColor: TColor);
    property ColumnRowTitlesFont: TFont read FColumnRowTitlesFont write SetColumnRowTitlesFont;
    property ColumnRowTitlesBGColor: TColor read GetColumnRowTitlesBGColor write SetColumnRowTitlesBGColor;

    property IsColumnRowTitlesExists: Boolean read GetIsRowTitlesColumnExists;
    property ColumnRowTitlesVisible: Boolean write SetColumnRowTitlesVisible;

    property IsBeginEditOnKeyPress: Boolean read FIsBeginEditOnKeyPress write FIsBeginEditOnKeyPress;
    property IsOneRowEditing: Boolean read FIsOneRowEditing write FIsOneRowEditing;
    property IsEditing: Boolean read FIsEditing;
    property IsSelected: Boolean read GetIsSelected;
    property SelectedRowIndex: Integer read FSelectedRowIndex;
    property SelectedColIndex: Integer read FSelectedColIndex;
    property SelectedText: String read GetSelectedText write SetSelectedText;
    property UnselectOnExit: Boolean read FUnselectOnExit write SetUnselectOnExit;
    property ShowZeros: Boolean read FShowZeros write SetShowZeros;

    property ColorColumnBorderColor: TColor read FColorColumnBorderColor write FColorColumnBorderColor;
    property ColorColumnCellMargin: Integer read FColorColumnCellMargin write FColorColumnCellMargin;

    property OnEdititingDone: TVSTEdititingDoneEvent read FOnEdititingDone write FOnEdititingDone;
    property OnEdititingBegin: TVSTEdititingBeginEvent read FOnEdititingBegin write FOnEdititingBegin;
  end;

  { TVSTSimpleTable }

  TVSTSimpleTable = class(TVSTCustomSimpleTable)
  private
    function GetColCount: Integer;
    function GetRowCount: Integer;
    procedure SetColumnVisibles(AValue: TBoolVector);
  protected
    FSelected: TBoolVector;
    FColumnVisibles: TBoolVector;

    function GetIsSelected: Boolean;
    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;

    procedure HeaderClear; override;

    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);
    function IsIndexCorrect(const AIndex: Integer): Boolean;
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor  Destroy; override;

    procedure ValuesClear; override;
    procedure Draw; override;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter); override;
    procedure SetColumn(const AColIndex: Integer; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetColumn(const ACaption: String; const AValues: TStrVector;
                        const AValuesAlignment: TAlignment = taCenter);

    procedure Show(const AIndex: Integer);

    property IsSelected: Boolean read GetIsSelected;

    property RowCount: Integer read GetRowCount;
    property ColCount: Integer read GetColCount;

    property ColumnVisibles: TBoolVector read FColumnVisibles write SetColumnVisibles;
  end;

  { TVSTTable }

  TVSTTable = class(TVSTSimpleTable)
  protected
    FAutosizeRowHeights: Boolean;
    FOnDelKeyDown: TVSTEvent;
    FOnReturnKeyDown: TVSTEvent;

    procedure SelectNode(Node: PVirtualNode);
    procedure UnselectNode;

    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure KeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
                          Node: PVirtualNode; var NodeHeight: Integer);

    procedure SetCanSelect(AValue: Boolean); override;
    function GetSelectedIndex: Integer;

    procedure MoveSelection(const ADeltaIndex: Integer);

    procedure SetAutosizeRowHeights(AValue: Boolean);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor  Destroy; override;

    procedure ValuesClear; override;

    procedure UnSelect;
    procedure Select(const AIndex: Integer);
    procedure Select(const AColIndex: Integer; const AValue: String);
    procedure Select(const AColumnCaption, AValue: String);
    property SelectedIndex: Integer read GetSelectedIndex;
    function ReSelect(const AIDVector: TIntVector; const AIDValue: Integer;
                      const AFirstRowSelectIfNotFound: Boolean = False): Boolean;
    function ReSelect(const AIDVector: TDateVector; const AIDValue: TDate;
                      const AFirstRowSelectIfNotFound: Boolean = False): Boolean;

    procedure Save(const AColumnTypes: TVSTColumnTypes;
                   const ASheetName: String = 'Лист1';
                   const ADoneMessage: String = 'Выполнено!';
                   const ALandscape: Boolean = False);

    property AutosizeRowHeights: Boolean read FAutosizeRowHeights write SetAutosizeRowHeights;
    property OnDelKeyDown: TVSTEvent read FOnDelKeyDown write FOnDelKeyDown;
    property OnReturnKeyDown: TVSTEvent read FOnReturnKeyDown write FOnReturnKeyDown;
  end;


  { TVSTCheckTable }

  TVSTCheckTable = class(TVSTSimpleTable)
  protected
    FOnCheck: TVSTCheckEvent;
    FMaxCheckedCount: Integer;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var {%H-}Allowed: Boolean);

    function GetIsAllChecked: Boolean;
    function GetIsAllUnchecked: Boolean;
    procedure SetMaxCheckedCount(AValue: Integer);

    procedure SetCanSelect(AValue: Boolean); override;

    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    function GetChecked(AIndex: Integer): Boolean;
    function GetSelected: TBoolVector;
    procedure SetSelected(AValue: TBoolVector);

    function GetCheckedCount: Integer;
    function GetUncheckedCount: Integer;

    procedure CheckNode(Node: PVirtualNode; const AChecked: Boolean);
    procedure Check(Node: PVirtualNode);
    procedure Check(const AIndex: Integer);
    procedure Uncheck(Node: PVirtualNode);
    procedure Uncheck(const AIndex: Integer);
    procedure ReverseCheckState(Node: PVirtualNode);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor  Destroy; override;
    procedure ValuesClear; override;
    procedure Draw; override;

    procedure CheckAll(const AChecked: Boolean);

    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
    property IsAllChecked: Boolean read GetIsAllChecked;
    property IsAllUnchecked: Boolean read GetIsAllUnchecked;
    property Selected: TBoolVector read GetSelected write SetSelected;

    property CheckedCount: Integer read GetCheckedCount;
    property UncheckedCount: Integer read GetUncheckedCount;
    property MaxCheckedCount: Integer read FMaxCheckedCount write SetMaxCheckedCount;
    procedure MaxCheckedCountClear;

    property OnCheck: TVSTCheckEvent read FOnCheck write FOnCheck;
  end;

  { TVSTCustomCategoryTable }

  TVSTCustomCategoryTable = class(TVSTCoreTable)
  protected
    FCategoryValues: TStrVector;
    FDataValues: TStrMatrix3D;
    FSelected: TBoolMatrix;

    procedure SetTreeLinesVisible(AValue: Boolean);
    procedure HeaderClear; override;
    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                      Column: TColumnIndex; {%H-}TextType: TVSTTextType;
                      var CellText: String);
    function IsIndexesCorrect(const AIndex1, AIndex2: Integer): Boolean;

    function IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    function GetIsSelected: Boolean;
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor  Destroy; override;

    procedure AddColumn(const ACaption: String; const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter); override;
    procedure SetColumn(const AColIndex: Integer; const AValues: TStrMatrix;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetColumn(const ACaption: String; const AValues: TStrMatrix;
                        const AValuesAlignment: TAlignment = taCenter);
    procedure SetCategories(const AValues: TStrVector);

    procedure Clear;
    procedure ValuesClear; override;

    procedure Draw;
    procedure ExpandAll(const AExpand: Boolean);
    procedure Show(const AIndex1, AIndex2: Integer);

    property TreeLinesVisible: Boolean write SetTreeLinesVisible;
    property IsSelected: Boolean read GetIsSelected;
  end;

  { TVSTCategoryRadioButtonTable }

  TVSTCategoryRadioButtonTable = class(TVSTCustomCategoryTable)
  protected

    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);

    procedure SetCanSelect(AValue: Boolean); override;
    procedure SelectNode(Node: PVirtualNode);
    procedure UnselectNode;
    function GetSelectedIndex1: Integer;
    function GetSelectedIndex2: Integer;
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor  Destroy; override;

    procedure Draw;

    procedure ValuesClear; override;

    procedure Select(const AIndex1, AIndex2: Integer);
    procedure SelectedIndexes(out AIndex1, AIndex2: Integer);
    property SelectedIndex1: Integer read GetSelectedIndex1;
    property SelectedIndex2: Integer read GetSelectedIndex2;
  end;

  { TVSTCategoryCheckTable }

  TVSTCategoryCheckTable = class(TVSTCustomCategoryTable)
  protected

    procedure InitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var {%H-}InitialStates: TVirtualNodeInitStates);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
                        {%H-}Shift: TShiftState; X, Y: Integer);
    procedure Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var {%H-}Allowed: Boolean);

    procedure SetCanSelect(AValue: Boolean); override;

    procedure SetChecked(AIndex1, AIndex2: Integer; AValue: Boolean);
    function GetChecked(AIndex1, AIndex2: Integer): Boolean;

    procedure CheckNode(Node: PVirtualNode; const AChecked: Boolean);
    procedure Check(Node: PVirtualNode);
    procedure Check(const AIndex1, AIndex2: Integer);
    procedure Uncheck(Node: PVirtualNode);
    procedure Uncheck(const AIndex1, AIndex2: Integer);
    procedure ReverseCheckState(Node: PVirtualNode);
    procedure ReverseCategoryCheckState(Node: PVirtualNode);

    function IsAllCheckedCategory(Node: PVirtualNode): Boolean;
    function IsAllUncheckedCategory(Node: PVirtualNode): Boolean;
    function IsHasCheckedCategory(Node: PVirtualNode): Boolean;
    function IsHasCheckedCategory(const AIndex: Integer): Boolean;
    procedure CheckCategory(Node: PVirtualNode; const AChecked: Boolean);
    procedure SetCategoryCheckState(Node: PVirtualNode);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor  Destroy; override;

    function IsCategoryAllChecked(const AIndex: Integer): Boolean;
    function IsCategoryAllUnchecked(const AIndex: Integer): Boolean;

    procedure CheckAll(const AChecked: Boolean);
    procedure CheckCategory(const AIndex: Integer; const AChecked: Boolean);

    property Checked[AIndex1, AIndex2: Integer]: Boolean read GetChecked write SetChecked;
  end;

implementation

{ TVSTEdit }

function TVSTEdit.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  Result:= (Node^.Index=FSelectedRowIndex) and (Column=FSelectedColIndex);
end;

function TVSTEdit.GetColumnRowTitlesBGColor: TColor;
begin
  Result:= clNone;
  if not IsColumnRowTitlesExists then Exit;
  Result:= FColumnValuesBGColors[FTitleColumnIndex];
end;

procedure TVSTEdit.SetColumnRowTitlesBGColor(AValue: TColor);
begin
  if not IsColumnRowTitlesExists then Exit;
  if FColumnValuesBGColors[FTitleColumnIndex]=AValue then Exit;
  FColumnValuesBGColors[FTitleColumnIndex]:= AValue;
  FTree.Refresh;
end;

procedure TVSTEdit.SetColumnRowTitlesFont(AValue: TFont);
begin
  FColumnRowTitlesFont.Assign(AValue);
  FTree.Refresh;
end;

procedure TVSTEdit.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  i, n: Integer;
begin
  if High(FDataValues)<Column then Exit;
  i:= Node^.Index;
  CellText:= EmptyStr;
  if VIsNil(FDataValues[Column]) then Exit;
  if FColumnTypes[Column]=ctKeyPick then
  begin
    if SEmpty(FDataValues[Column, i]) then Exit;
    n:= VIndexOf(FKeys[Column], StrToInt(FDataValues[Column, i]));
    if n>=0 then
      CellText:= FPicks[Column, n]
    else
      CellText:= FDataValues[Column, i];
  end
  else begin
    if (FDataValues[Column, i]='0') and (not ShowZeros) then Exit;
    CellText:= FDataValues[Column, i];
  end;
end;

procedure TVSTEdit.Select(const ARowIndex, AColIndex: Integer);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(ARowIndex);
  if not Assigned(Node) then Exit;
  if not IsEditingColIndexCorrect(AColIndex) then Exit;
  SelectCell(Node, AColIndex);
end;

procedure TVSTEdit.Select(const ARowIndex: Integer; const AColumnCaption: String);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, AColumnCaption);
  if not IsEditingColIndexCorrect(ColIndex) then Exit;
  Select(ARowIndex, ColIndex);
end;

procedure TVSTEdit.Select(const ARowTitle, AColumnCaption: String);
var
  RowIndex: Integer;
begin
  if not IsColumnRowTitlesExists then Exit;
  RowIndex:= VIndexOf(FDataValues[FTitleColumnIndex], ARowTitle);
  if not IsRowIndexCorrect(RowIndex) then Exit;
  Select(RowIndex, AColumnCaption);
end;

procedure TVSTEdit.Show(const ARowIndex: Integer);
var
  Node: PVirtualNode;
begin
  if not IsRowIndexCorrect(ARowIndex) then Exit;
  Node:= NodeFromIndex(ARowIndex);
  if not Assigned(Node) then Exit;
  FTree.FocusedNode:= Node;
end;

function TVSTEdit.IsColumnValuesExists(const AColIndex: Integer): Boolean;
begin
  Result:= False;
  if not IsEditingColIndexCorrect(AColIndex) then Exit;
  if MIsNil(FDataValues) then Exit;
  if VIsNil(FDataValues[AColIndex]) then Exit;
  Result:= True;
end;

function TVSTEdit.ColumnAsInteger(out AValues: TIntVector;
  const AColIndex: Integer; const ADefaultValue: Integer): Boolean;
var
  i: Integer;
  Value: Integer;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
    if TryStrToInt(FDataValues[AColIndex,i], Value) then
      AValues[i]:= Value;
end;

function TVSTEdit.ColumnAsString(out AValues: TStrVector;
  const AColIndex: Integer; const ADefaultValue: String): Boolean;
var
  i: Integer;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
      AValues[i]:= FDataValues[AColIndex,i];
end;

function TVSTEdit.ColumnAsDate(out AValues: TDblVector;
  const AColIndex: Integer; const ADefaultValue: TDate): Boolean;
var
  i: Integer;
  Value: TDate;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
    if TryStrToDate(FDataValues[AColIndex,i], Value) then
      AValues[i]:= Value;
end;

function TVSTEdit.ColumnAsTime(out AValues: TDblVector;
  const AColIndex: Integer; const ADefaultValue: TTime): Boolean;
var
  i: Integer;
  Value: TTime;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
    if TryStrToTime(FDataValues[AColIndex,i], Value) then
      AValues[i]:= Value;
end;

function TVSTEdit.ColumnAsColor(out AValues: TColorVector;
  const AColIndex: Integer; const ADefaultValue: TColor): Boolean;
var
  i: Integer;
  Value: Integer;
begin
  AValues:= nil;
  Result:= IsColumnValuesExists(AColIndex);
  if not Result then Exit;
  VDim(AValues, Length(FDataValues[AColIndex]), ADefaultValue);
  for i:= 0 to High(FDataValues[AColIndex]) do
    if TryStrToInt(FDataValues[AColIndex,i], Value) then
      AValues[i]:= TColor(Value);
end;

function TVSTEdit.ColumnAsInteger(out AValues: TIntVector;
  const ACaption: String; const ADefaultValue: Integer): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsInteger(AValues, ColIndex, ADefaultValue);
end;

function TVSTEdit.ColumnAsString(out AValues: TStrVector;
  const ACaption: String; const ADefaultValue: String): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsString(AValues, ColIndex, ADefaultValue);
end;

function TVSTEdit.ColumnAsDate(out AValues: TDblVector; const ACaption: String;
  const ADefaultValue: TDate): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsDate(AValues, ColIndex, ADefaultValue);
end;

function TVSTEdit.ColumnAsTime(out AValues: TDblVector; const ACaption: String;
  const ADefaultValue: TTime): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsTime(AValues, ColIndex, ADefaultValue);
end;

function TVSTEdit.ColumnAsColor(out AValues: TColorVector;
  const ACaption: String; const ADefaultValue: TColor): Boolean;
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeadercaptions, ACaption);
  Result:= ColumnAsColor(AValues, ColIndex, ADefaultValue);
end;

procedure TVSTEdit.RowInsert(const ARowIndex: Integer; const AValues: TStrVector = nil);
var
  Node: PVirtualNode;
  i: Integer;
begin
  MRowIns(FDataValues, ARowIndex, AValues);
  for i:= 0 to High(FDataValues) do
  begin
    if FColumnTypes[i]=ctKeyPick then
      FDataValues[i, ARowIndex]:= IntToStr(FKeys[i, 0]);
  end;
  Node:= NodeFromIndex(ARowIndex);
  if Assigned(Node) then
    FTree.InsertNode(Node, amInsertBefore)
  else
    FTree.AddChild(FTree.RootNode);
  FTree.Refresh;
end;

procedure TVSTEdit.RowDelete(const ARowIndex: Integer);
var
  Node: PVirtualNode;
begin
  if not IsRowIndexCorrect(ARowIndex) then Exit;
  Node:= NodeFromIndex(ARowIndex);
  if not Assigned(Node) then Exit;
  MRowDel(FDataValues, ARowIndex);
  FTree.DeleteNode(Node);
  FTree.Refresh;
end;

function TVSTEdit.GetRowValues(const ARowIndex: Integer): TStrVector;
begin
  Result:= nil;
  if not IsRowIndexCorrect(ARowIndex) then Exit;
  Result:= MRowGet(FDataValues, ARowIndex);
end;

procedure TVSTEdit.SetRowValues(const ARowIndex: Integer; const ARowValues: TStrVector);
begin
  MRowSet(FDataValues, ARowIndex, ARowValues);
  FTree.Refresh;
end;

procedure TVSTEdit.SetColumnRowTitlesHeaderBGColor(const ABGColor: TColor);
begin
  if not IsColumnRowTitlesExists then Exit;
  SetColumnHeaderBGColor(FTitleColumnIndex, ABGColor);
end;

procedure TVSTEdit.SetShowZeros(AValue: Boolean);
begin
  if FShowZeros=AValue then Exit;
  FShowZeros:= AValue;
  FTree.Refresh;
end;

procedure TVSTEdit.SetUnselectOnExit(AValue: Boolean);
begin
  if FUnselectOnExit=AValue then Exit;
  FUnselectOnExit:= AValue;
end;

function TVSTEdit.CellRectangle(Column: TColumnIndex; ACellRect: TRect): TRect;
begin
  Result:= inherited CellRectangle(Column, ACellRect);
  if FColumnTypes[Column]=ctColor then
    Result.Inflate(-FColorColumnCellMargin,-FColorColumnCellMargin,
                   -FColorColumnCellMargin,-FColorColumnCellMargin);
end;

function TVSTEdit.CellGridColor(Node: PVirtualNode; Column: TColumnIndex; ABGColor: TColor): TColor;
begin
  Result:= inherited CellGridColor(Node, Column, ABGColor);
  if (FColumnTypes[Column]=ctColor) and (ColorColumnBorderColor<>clNone) then
    Result:= FColorColumnBorderColor;
end;

procedure TVSTEdit.SelectCell(Node: PVirtualNode; Column: TColumnIndex; const ASaveChanges: Boolean = True);
var
  RowIndex, ColIndex: Integer;
begin
  if (not Assigned(Node)) and (Column=-1) then  //unselect
  begin
    if IsEditing and IsOneRowEditing then Exit;
    EndEdit(ASaveChanges);
    if FCanUnselect then
    begin
      FSelectedRowIndex:= -1;
      FSelectedColIndex:= -1;
    end;
  end
  else if Assigned(Node) and (Column<>FTitleColumnIndex) then //select
  begin
    if IsEditing and IsOneRowEditing and (Node^.Index<>FSelectedRowIndex) then Exit;
    EndEdit(ASaveChanges);
    RowIndex:= FSelectedRowIndex;
    ColIndex:= FSelectedColIndex;
    FSelectedRowIndex:= Node^.Index;
    FSelectedColIndex:= Column;
    FTree.FocusedNode:= Node;
    if (FSelectedRowIndex=RowIndex) and (FSelectedColIndex=ColIndex) then
      BeginEdit;
  end;

  if Assigned(FOnSelect) then FOnSelect;
  FTree.Refresh;
end;

procedure TVSTEdit.UnselectCell(const ASaveChanges: Boolean = True);
begin
  SelectCell(nil, -1, ASaveChanges);
end;

procedure TVSTEdit.NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  SelectCell(HitInfo.HitNode, HitInfo.HitColumn);
end;

procedure TVSTEdit.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then
  begin
    if FCanUnselect then
      UnselectCell;
  end
end;

procedure TVSTEdit.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  R: Integer;
begin
  if FColumnTypes[FSelectedColIndex]=ctColor then
  begin
    Key:= VK_UNKNOWN;
    Exit;
  end;

  if Key=VK_RETURN then
  begin
    R:= FSelectedRowIndex + 1;
    //if (R<0) or (R>High(FDataValues[FTitleColumnIndex])) then
    if not IsRowIndexCorrect(R) then
      EditorExit
    else begin
      PostMessage(FTree.Handle, LM_KEYDOWN, VK_DOWN, 0);
      PostMessage(FTree.Handle, LM_KEYDOWN, VK_RETURN, 0);
    end;
  end
  else if Key=VK_ESCAPE then
    EditorExit;
end;

procedure TVSTEdit.EditorExit;
begin
  PostMessage(FTree.Handle, LM_KEYDOWN, VK_ESCAPE, 0);
  FTree.SetFocus;
end;

procedure TVSTEdit.TreeExit(Sender: TObject);
begin
  if CanUnselect then
  begin
    if UnselectOnExit then
      Unselect
    else
      EndEdit;
  end;


  //if CanUnselect and UnselectOnExit then
  //  UnSelect;
end;

procedure TVSTEdit.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not IsSelected then Exit;
  case Key of
   VK_RETURN: if IsBeginEditOnKeyPress then BeginEdit;
   VK_DELETE, VK_BACK: DeleteSelectedCellText;
   VK_ESCAPE: if Assigned(FEditor) then
                MoveSelectionVertical(0)
              else
                UnSelect;
   VK_DOWN: MoveSelectionVertical(1);
   VK_UP:   MoveSelectionVertical(-1);
   VK_RIGHT: MoveSelectionHorizontal(1);
   VK_LEFT: MoveSelectionHorizontal(-1);
  end;
end;

procedure TVSTEdit.UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  n: Integer;
  Key: Word;
begin
  if not IsSelected then Exit;
  if not IsBeginEditOnKeyPress then Exit;

  case FColumnTypes[FSelectedColIndex] of
  ctInteger:
    begin
      if SPos(SYMBOLS_DIGITS, UTF8Key)=0 then Exit;
      FDataValues[FSelectedColIndex, FSelectedRowIndex]:= UTF8Key;
      BeginEdit;
    end;
  ctString:
    begin
      if SPos(SYMBOLS_KEYBOARD, UTF8Key)=0 then Exit;
      FDataValues[FSelectedColIndex, FSelectedRowIndex]:= UTF8Key;
      BeginEdit;
    end;
  ctDate, ctTime:
    begin
      n:= SPos(SYMBOLS_DIGITS, UTF8Key);
      if n=0 then Exit;
      BeginEdit;
      Key:= VK_0;
      if n<10 then
        Key:= Key + n;
      PostMessage(FEditor.Handle, LM_KEYDOWN, Key, 0);
    end;
    ctDouble:
      begin
        if SPos(SYMBOLS_DIGITS+SYMBOL_COMMA+SYMBOL_DOT, UTF8Key)=0 then Exit;
        if SSame(UTF8Key, SYMBOL_COMMA) or SSame(UTF8Key, SYMBOL_DOT) then
          UTF8Key:= DefaultFormatSettings.DecimalSeparator;
        FDataValues[FSelectedColIndex, FSelectedRowIndex]:= UTF8Key;
        BeginEdit;
      end;
  //ctKeyPick: ; //not need
  //ctColor:   ; //not need
  end;

  FTree.Refresh;
end;

constructor TVSTEdit.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
  FTree.OnGetText:= @GetText;
  FTree.OnNodeClick:= @NodeClick;
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnKeyDown:= @KeyDown;
  FTree.OnUTF8KeyPress:= @UTF8KeyPress;
  FTree.OnExit:= @TreeExit;

  FColumnRowTitlesFont:= TFont.Create;
  FColumnRowTitlesFont.Assign(FTree.Font);

  FTitleColumnIndex:= -1;
  FSelectedRowIndex:= -1;
  FSelectedColIndex:= -1;
  FShowZeros:= False;
  FUnselectOnExit:= True;
  FIsEditing:= False;
  FIsOneRowEditing:= False;
  FIsBeginEditOnKeyPress:= True;
  FCellTextBeforeEditing:= 'FCellTextBeforeEditing';
  FColorColumnCellMargin:= 0;
  FColorColumnBorderColor:= clNone;
end;

destructor TVSTEdit.Destroy;
begin
  FreeAndNil(FColumnRowTitlesFont);
  if Assigned(FEditor) then FreeAndNil(FEditor);
  inherited Destroy;
end;

function TVSTEdit.GetIsRowTitlesColumnExists: Boolean;
begin
  Result:= FTitleColumnIndex>=0;
end;

function TVSTEdit.CellBGColor(Node: PVirtualNode; Column: TColumnIndex): TColor;
begin
  if FColumnTypes[Column]=ctColor then
    Result:= StrToInt(FDataValues[Column, Node^.Index])
  else
    Result:= inherited CellBGColor(Node, Column);
end;

procedure TVSTEdit.CellFont(Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column=FTitleColumnIndex then
    FCellFont.Assign(FColumnRowTitlesFont)
  else begin
    inherited CellFont(Node, Column);
    if FColumnTypes[Column]=ctColor then
      FCellFont.Color:= StrToInt(FDataValues[Column, Node^.Index]);
  end;
end;

procedure TVSTEdit.DeleteSelectedCellText;
begin
  FDataValues[FSelectedColIndex, FSelectedRowIndex]:= EmptyStr;
  FTree.Refresh;
end;

procedure TVSTEdit.MoveSelectionVertical(const ADirection: Integer);
var
  NewRowIndex: Integer;
begin
  if not IsSelected then Exit;
  NewRowIndex:= FSelectedRowIndex + ADirection;
  if not IsRowIndexCorrect(NewRowIndex) then Exit;
  Select(NewRowIndex, FSelectedColIndex);
end;

procedure TVSTEdit.MoveSelectionHorizontal(const ADirection: Integer);
var
  NewColIndex: Integer;
begin
  if not IsSelected then Exit;
  NewColIndex:= FSelectedColIndex;
  repeat
    NewColIndex:= NewColIndex + ADirection;
  until NewColIndex<>FTitleColumnIndex;
  if not IsColIndexCorrect(NewColIndex) then Exit;
  Select(FSelectedRowIndex, NewColIndex);
end;

procedure TVSTEdit.BeginEdit;
var
  ColumnType: TVSTColumnType;

  procedure CreateEditorInteger;
  begin
    FEditor:= TSpinEdit.Create(FTree);
    TSpinEdit(FEditor).Text:= SelectedText;
    TSpinEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure CreateEditorDouble;
  begin
    FEditor:= TFloatSpinEdit.Create(FTree);
    TFloatSpinEdit(FEditor).Text:= SelectedText;
    TFloatSpinEdit(FEditor).DecimalPlaces:= FDecimalPlaces[FSelectedColIndex];
    TFloatSpinEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure CreateEditorString;
  begin
    FEditor:= TEdit.Create(FTree);
    TEdit(FEditor).Text:= SelectedText;
    TEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure CreateEditorDate;
  begin
    FEditor:= TDateTimePicker.Create(FTree);
    TDateTimePicker(FEditor).Kind:= dtkDate;
    TDateTimePicker(FEditor).Alignment:=FTree.Header.Columns[FSelectedColIndex].Alignment;
    TDateTimePicker(FEditor).Date:= StrToDateDef(SelectedText, Date);
  end;

  procedure CreateEditorTime;
  begin
    FEditor:= TDateTimePicker.Create(FTree);
    TDateTimePicker(FEditor).Kind:= dtkTime;
    TDateTimePicker(FEditor).TimeDisplay:= tdHMSMs;
    TDateTimePicker(FEditor).TimeFormat:= tf24;
    TDateTimePicker(FEditor).Alignment:=FTree.Header.Columns[FSelectedColIndex].Alignment;
    TDateTimePicker(FEditor).Time:= StrToTimeDef(SelectedText, 0);
  end;

  procedure CreateEditorKeyPick;
  var
    n: Integer;

    procedure SetState(AState: TBCButtonState);
    begin
      AState.FontEx.Shadow:= False;
      AState.FontEx.Name:= ValuesFont.Name;
      AState.FontEx.Color:= ValuesFont.Color;
      AState.FontEx.Style:= ValuesFont.Style;
      AState.FontEx.Height:= - ValuesFont.Height;
      case FTree.Header.Columns[FSelectedColIndex].Alignment of
        taLeftJustify:  AState.FontEx.TextAlignment:= bcaLeftCenter;
        taRightJustify: AState.FontEx.TextAlignment:= bcaRightCenter;
        taCenter:       AState.FontEx.TextAlignment:= bcaCenter;
      end;
      AState.FontEx.PaddingLeft:= 5;
      AState.Border.Color:= clHighlight;
      AState.Border.Style:= bboSolid;
      AState.Background.Color:= FColumnValuesBGColors[FSelectedColIndex];
      AState.Background.Style:= bbsColor;
    end;
  begin
    FEditor:= TBCComboBox.Create(FTree);
    TBCComboBox(FEditor).Rounding.RoundX:= 0;
    TBCComboBox(FEditor).Rounding.RoundY:= 0;
    TBCComboBox(FEditor).DropDownBorderColor:= clHighlight;
    TBCComboBox(FEditor).FocusBorderColor:= clHighlight;
    SetState(TBCComboBox(FEditor).StateNormal);
    SetState(TBCComboBox(FEditor).StateHover);
    SetState(TBCComboBox(FEditor).StateClicked);
    VToStrings(FPicks[FSelectedColIndex], TBCComboBox(FEditor).Items);
    n:= VIndexOf(FKeys[FSelectedColIndex], StrToInt(SelectedText));
    if n>=0 then
      TBCComboBox(FEditor).ItemIndex:= n;
  end;

  procedure CreateEditorColor;
  begin
    FEditor:= TEdit.Create(FTree);
    TEdit(FEditor).Color:= StrToInt(SelectedText);
    TEdit(FEditor).Font.Color:= TEdit(FEditor).Color;
    TEdit(FEditor).Text:= SelectedText;
    TEdit(FEditor).Alignment:= FTree.Header.Columns[FSelectedColIndex].Alignment;
  end;

  procedure GoEditInteger;
  begin
    FEditor.SetFocus;
    TSpinEdit(FEditor).SelStart:= SLength(TSpinEdit(FEditor).Text);
  end;

  procedure GoEditDouble;
  begin
    FEditor.SetFocus;
    TFloatSpinEdit(FEditor).SelStart:= SLength(TFloatSpinEdit(FEditor).Text);
  end;

  procedure GoEditString;
  begin
    FEditor.SetFocus;
    TEdit(FEditor).SelStart:= SLength(TEdit(FEditor).Text);
  end;

  procedure GoEditDate;
  begin
    FEditor.SetFocus;
  end;

  procedure GoEditTime;
  begin
    FEditor.SetFocus;
  end;

  procedure GoEditKeyPick;
  begin
    FEditor.SetFocus;
  end;

  procedure GoEditColor;
  var
    ColorDialog: TColorDialog;
  begin
    ColorDialog:= TColorDialog.Create(nil);
    try
      if ColorDialog.Execute then
      begin
        TEdit(FEditor).Text:= IntToStr(ColorToRGB(ColorDialog.Color));
        TEdit(FEditor).Color:= ColorDialog.Color;
        TEdit(FEditor).Font.Color:= ColorDialog.Color;
      end;
      Unselect;
    finally
      FreeAndNil(ColorDialog);
    end;
  end;

begin
  FIsEditing:= True;
  ColumnType:= FColumnTypes[FSelectedColIndex];

  if Assigned(FOnEdititingBegin) then
    FOnEdititingBegin;

  if FCellTextBeforeEditing='FCellTextBeforeEditing' then
  begin
    FCellTextBeforeEditing:= SelectedText;
    if Assigned(FEditor) then FreeAndNil(FEditor);
    case ColumnType of
      ctInteger: CreateEditorInteger;
      ctString:  CreateEditorString;
      ctDate:    CreateEditorDate;
      ctTime:    CreateEditorTime;
      ctKeyPick: CreateEditorKeyPick;
      ctColor:   CreateEditorColor;
      ctDouble:  CreateEditorDouble;
    else
      Exit;
    end;
    FEditor.OnKeyDown:= @EditorKeyDown;
    FEditor.Parent:= FTree;
    FEditor.AutoSize:= False;
    FEditor.BoundsRect:= SelectedCellRect;
    FEditor.Show;
  end;

  case ColumnType of
    ctInteger: GoEditInteger;
    ctString:  GoEditString;
    ctDate:    GoEditDate;
    ctTime:    GoEditTime;
    ctKeyPick: GoEditKeyPick;
    ctColor:   GoEditColor;
    ctDouble:  GoEditDouble;
  end;
end;

procedure TVSTEdit.EndEdit(const ASaveChanges: Boolean);
var
  ColumnType: TVSTColumnType;
begin
  if not Assigned(FEditor) then Exit;

  ColumnType:= FColumnTypes[FSelectedColIndex];
  if ASaveChanges then
  begin
    case ColumnType of
      ctInteger: if TSpinEdit(FEditor).Value=0 then
                   SelectedText:= EmptyStr
                 else
                   SelectedText:= IntToStr(TSpinEdit(FEditor).Value);
      ctString:  SelectedText:= TEdit(FEditor).Text;
      ctDate:    SelectedText:= FormatDateTime(
                                      FColumnFormatStrings[FSelectedColIndex],
                                      TDateTimePicker(FEditor).Date);
      ctTime:    SelectedText:= FormatDateTime(
                                      FColumnFormatStrings[FSelectedColIndex],
                                      TDateTimePicker(FEditor).Time);
      ctKeyPick: SelectedText:= IntToStr(FKeys[FSelectedColIndex, TBCComboBox(FEditor).ItemIndex]);
      ctColor:   SelectedText:= TEdit(FEditor).Text;
      ctDouble:  if TFloatSpinEdit(FEditor).Value=0 then
                   SelectedText:= EmptyStr
                 else
                   SelectedText:= FloatToStr(TFloatSpinEdit(FEditor).Value);
    end;
  end
  else begin
    SelectedText:= FCellTextBeforeEditing;
  end;
  FCellTextBeforeEditing:= 'FCellTextBeforeEditing';

  if Assigned(FOnEdititingDone) then
    FOnEdititingDone(FSelectedRowIndex, FSelectedColIndex, SelectedText, ColumnType, ASaveChanges);

  FIsEditing:= False;
  FreeAndNil(FEditor);
end;

procedure TVSTEdit.SetColumnRowTitlesVisible(AValue: Boolean);
begin
  if not IsColumnRowTitlesExists then Exit;
  if AValue then
    FTree.Header.Columns[FTitleColumnIndex].Options:=
      FTree.Header.Columns[FTitleColumnIndex].Options + [coVisible]
  else
    FTree.Header.Columns[FTitleColumnIndex].Options:=
      FTree.Header.Columns[FTitleColumnIndex].Options - [coVisible];
  FTree.Refresh;
end;

function TVSTEdit.GetIsSelected: Boolean;
begin
  Result:= False;
  if not Assigned(Self) then Exit;
  Result:= (FSelectedRowIndex>=0) and (FSelectedColIndex>=0);
end;

function TVSTEdit.GetSelectedText: String;
begin
  Result:= EmptyStr;
  if not IsSelected then Exit;
  Result:= FDataValues[FSelectedColIndex, FSelectedRowIndex];
end;

procedure TVSTEdit.SetSelectedText(AValue: String);
begin
  if not IsSelected then Exit;
  if FDataValues[FSelectedColIndex, FSelectedRowIndex]=AValue then Exit;
  FDataValues[FSelectedColIndex, FSelectedRowIndex]:= AValue;
  FTree.Refresh;
end;

procedure TVSTEdit.HeaderClear;
begin
  inherited HeaderClear;
  FDataValues:= nil;
  FColumnTypes:= nil;
  FColumnFormatStrings:= nil;
  FDecimalPlaces:= nil;
  FTitleColumnIndex:= -1;
end;

procedure TVSTEdit.ValuesClear;
begin
  FSelectedRowIndex:= -1;
  FSelectedColIndex:= -1;
  inherited ValuesClear;
end;

procedure TVSTEdit.Draw;
var
  i, n: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;

  if IsColumnRowTitlesExists then
  begin
    n:= Length(FDataValues[FTitleColumnIndex]);
    for i:= 0 to High(FDataValues) do
      if i<>FTitleColumnIndex then
        VReDim(FDataValues[i], n, EmptyStr);
    VSTLoad(FTree, FDataValues[FTitleColumnIndex]);
  end;

  inherited Draw;
end;

procedure TVSTEdit.AutosizeColumnRowTitlesEnable;
begin
  if not IsColumnRowTitlesExists then Exit;
  AutosizeColumnEnable(FTitleColumnIndex);
end;

function TVSTEdit.SelectedCellRect: TRect;
var
  Node: PVirtualNode;
begin
  Result:= Rect(0,0,0,0);
  if not IsSelected then Exit;
  Node:= NodeFromIndex(FSelectedRowIndex);
  Result:= FTree.GetDisplayRect(Node, FSelectedColIndex, False);
  if IsColumnRowTitlesExists then
    if FSelectedColIndex=FTitleColumnIndex+1 then
      Result.Left:= Result.Left + 1;
  Result.Bottom:= Result.Bottom - 1;
  if Result.Top<=0 then Result.Top:= 1;
end;

function TVSTEdit.IsEditingColIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= (AIndex<>FTitleColumnIndex) and IsColIndexCorrect(AIndex);
end;

function TVSTEdit.IsRowIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if not IsColumnRowTitlesExists then Exit;
  if MIsNil(FDataValues) then Exit;
  Result:= (AIndex>=0) and (AIndex<=High(FDataValues[FTitleColumnIndex]));
end;

procedure TVSTEdit.AddColumnRowTitles(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
begin
  if IsColumnRowTitlesExists then Exit;
  AddColumn(ACaption, AWidth, ACaptionAlignment);
  FTitleColumnIndex:= High(FHeaderCaptions);
  SetNewColumnSettings(ctUndefined, EmptyStr);
  MAppend(FKeys, nil);
  MAppend(FPicks, nil);
end;

procedure TVSTEdit.SetColumnRowTitles(const AValues: TStrVector;
  const AValuesAlignment: TAlignment);
begin
  if not IsColumnRowTitlesExists then Exit;
  FDataValues[FTitleColumnIndex]:= VCut(AValues);
  FTree.Header.Columns[FTitleColumnIndex].Alignment:= AValuesAlignment;
  FTree.Refresh;
end;

procedure TVSTEdit.SetNewColumnSettings(const AColumnType: TVSTColumnType;
  const AFormatString: String; const ADecimalPlaces: Integer = 0);
begin
  MAppend(FDataValues, nil);
  VAppend(FColumnFormatStrings, AFormatString);
  VAppend(FDecimalPlaces, ADecimalPlaces);
  SetLength(FColumnTypes, Length(FHeaderCaptions));
  FColumnTypes[High(FColumnTypes)]:= AColumnType;
end;

procedure TVSTEdit.AddColumnInteger(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment;
  const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctInteger, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddColumnDouble(const ACaption: String;
                        const ADecimalPlaces: Integer = 2;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
begin
  AddValuesColumn(ctDouble, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment,
                  nil, nil, ADecimalPlaces);
end;

procedure TVSTEdit.AddColumnString(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment;
  const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctString, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddColumnDate(const ACaption, AFormatString: String;
  const AWidth: Integer;
  const ACaptionAlignment: TAlignment; const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctDate, ACaption, AFormatString, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddColumnTime(const ACaption, AFormatString: String;
  const AWidth: Integer;
  const ACaptionAlignment: TAlignment; const AValuesAlignment: TAlignment);
begin
  AddValuesColumn(ctTime, ACaption, AFormatString, AWidth, ACaptionAlignment, AValuesAlignment);
end;

procedure TVSTEdit.AddColumnKeyPick(const ACaption: String;
                        const AKeys: TIntVector; const APicks: TStrVector;
                        const AWidth: Integer = 100;
                        const ACaptionAlignment: TAlignment = taCenter;
                        const AValuesAlignment: TAlignment = taCenter);
begin
  AddValuesColumn(ctKeyPick, ACaption, EmptyStr, AWidth, ACaptionAlignment, AValuesAlignment, AKeys, APicks);
end;

procedure TVSTEdit.AddColumnColor(const ACaption: String; const AWidth: Integer;
                                  const ACaptionAlignment: TAlignment = taCenter);
begin
  AddValuesColumn(ctColor, ACaption, EmptyStr, AWidth, ACaptionAlignment);
end;

procedure TVSTEdit.SetColumnInteger(const ACaption: String; const AValues: TIntVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnInteger(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnDouble(const ACaption: String; const AValues: TDblVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnDouble(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnString(const ACaption: String; const AValues: TStrVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnString(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnDate(const ACaption: String; const AValues: TDateVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnDate(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnTime(const ACaption: String; const AValues: TTimeVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnTime(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnColor(const ACaption: String; const AValues: TColorVector);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  SetColumnColor(ColIndex, AValues);
end;

procedure TVSTEdit.SetColumnInteger(const AColIndex: Integer; const AValues: TIntVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VIntToStr(AValues);
end;

procedure TVSTEdit.SetColumnDouble(const AColIndex: Integer; const AValues: TDblVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VFloatToStr(AValues);
end;

procedure TVSTEdit.SetColumnString(const AColIndex: Integer; const AValues: TStrVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VCut(AValues);
end;

procedure TVSTEdit.SetColumnDate(const AColIndex: Integer; const AValues: TDateVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VFormatDateTime(FColumnFormatStrings[AColIndex], AValues);
end;

procedure TVSTEdit.SetColumnTime(const AColIndex: Integer; const AValues: TTimeVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VFormatDateTime(FColumnFormatStrings[AColIndex], AValues);
end;

procedure TVSTEdit.SetColumnColor(const AColIndex: Integer; const AValues: TColorVector);
begin
  if not IsColIndexCorrect(AColIndex) then Exit;
  FDataValues[AColIndex]:= VIntToStr(AValues);
end;

procedure TVSTEdit.AddValuesColumn(const AColumnType: TVSTColumnType;
                             const ACaption, AFormatString: String;
                             const AWidth: Integer;
                             const ACaptionAlignment: TAlignment;
                             const AValuesAlignment: TAlignment;
                             const AKeys: TIntVector = nil;
                             const APicks: TStrVector = nil;
                             const ADecimalPlaces: Integer = 0);
begin
  AddColumn(ACaption, AWidth, ACaptionAlignment);
  FTree.Header.Columns[High(FHeaderCaptions)].Alignment:= AValuesAlignment;
  SetNewColumnSettings(AColumnType, AFormatString, ADecimalPlaces);
  MAppend(FKeys, AKeys);
  MAppend(FPicks, APicks);
end;

procedure TVSTEdit.UnSelect(const ASaveChanges: Boolean);
begin
  if IsSelected then
    UnselectCell(ASaveChanges);
end;

{ TVSTCategoryCheckTable }

function TVSTCategoryCheckTable.GetChecked(AIndex1, AIndex2: Integer): Boolean;
begin
  Result:= False;
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  Result:= FSelected[AIndex1, AIndex2];
end;

procedure TVSTCategoryCheckTable.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if FTree.GetNodeLevel(Node)=0 then
  begin
    Node^.CheckType:= ctTriStateCheckBox;
    Node^.CheckState:= csUncheckedNormal;
  end
  else if FTree.GetNodeLevel(Node)=1 then
  begin
    Node^.CheckType:= ctCheckBox;
    Node^.CheckState:= csUncheckedNormal;
  end;
end;

procedure TVSTCategoryCheckTable.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  Node:= FTree.GetNodeAt(X, Y);
  if not Assigned(Node) then Exit;

  if Button=mbRight then
  begin
    if FCanUnselect then
      CheckAll(False);
  end
  else if Button=mbLeft then
  begin
    if FTree.GetNodeLevel(Node)=1 then
    begin
      ReverseCheckState(Node);
      SetCategoryCheckState(Node^.Parent);
    end
    else if FTree.GetNodeLevel(Node)=0 then
      ReverseCategoryCheckState(Node);
  end;

  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.Checking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  if FTree.GetNodeLevel(Node) = 1 then
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
    SetCategoryCheckState(Node^.Parent);
  end
  else if FTree.GetNodeLevel(Node) = 0 then
  begin
    if IsAllCheckedCategory(Node) then
    begin
      CheckCategory(Node, True);
      NewState:= csCheckedNormal;
    end
    else if IsAllUncheckedCategory(Node) then
    begin
      CheckCategory(Node, False);
      NewState:= csUncheckedNormal;
    end
    else begin
      if NewState=csUncheckedNormal then
      begin
        CheckCategory(Node, True);
        NewState:= csCheckedNormal;
      end
      else if NewState=csCheckedNormal then
      begin
        CheckCategory(Node, False);
        NewState:= csUncheckedNormal;
      end;
    end;
  end;
end;

procedure TVSTCategoryCheckTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then CheckAll(False);
  inherited SetCanSelect(AValue);
end;

procedure TVSTCategoryCheckTable.CheckNode(Node: PVirtualNode; const AChecked: Boolean);
begin
  if (not Assigned(Node)) or MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[(Node^.Parent)^.Index]) then Exit;
  if AChecked then
    Node^.CheckState:= csCheckedNormal
  else
    Node^.CheckState:= csUnCheckedNormal;
  FSelected[(Node^.Parent)^.Index, Node^.Index]:= AChecked;
  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.Check(Node: PVirtualNode);
begin
  CheckNode(Node, True);
end;

procedure TVSTCategoryCheckTable.Check(const AIndex1, AIndex2: Integer);
begin
  Check(NodeFromIndex(AIndex1, AIndex2));
end;

procedure TVSTCategoryCheckTable.SetChecked(AIndex1, AIndex2: Integer;
  AValue: Boolean);
begin
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  if Avalue then
    Check(AIndex1, AIndex2)
  else
    Uncheck(AIndex1, AIndex2);
end;

procedure TVSTCategoryCheckTable.Uncheck(Node: PVirtualNode);
begin
  CheckNode(Node, False);
end;

procedure TVSTCategoryCheckTable.Uncheck(const AIndex1, AIndex2: Integer);
begin
  Uncheck(NodeFromIndex(AIndex1, AIndex2));
end;

procedure TVSTCategoryCheckTable.ReverseCheckState(Node: PVirtualNode);
begin
  if Node^.CheckState=csUncheckedNormal then
    Check(Node)
  else if Node^.CheckState=csCheckedNormal then
    Uncheck(Node);
end;

procedure TVSTCategoryCheckTable.ReverseCategoryCheckState(Node: PVirtualNode);
begin
  if Node^.CheckState=csUncheckedNormal then
    CheckCategory(Node, True)
  else if Node^.CheckState=csCheckedNormal then
    CheckCategory(Node, False);
end;





constructor TVSTCategoryCheckTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnInitNode:= @InitNode;
  FTree.OnChecking:= @Checking;
end;

destructor TVSTCategoryCheckTable.Destroy;
begin
  inherited Destroy;
end;

function TVSTCategoryCheckTable.IsHasCheckedCategory(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsHasCheckedCategory(Node^.Index);
end;

function TVSTCategoryCheckTable.IsHasCheckedCategory(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[AIndex]) then Exit;
  Result:= VIsTrue(FSelected[AIndex]);
end;

function TVSTCategoryCheckTable.IsCategoryAllChecked(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[AIndex]) then Exit;
  Result:= VIsAllTrue(FSelected[AIndex]);
end;

function TVSTCategoryCheckTable.IsCategoryAllUnchecked(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[AIndex]) then Exit;
  Result:= VIsAllFalse(FSelected[AIndex]);
end;

function TVSTCategoryCheckTable.IsAllCheckedCategory(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsCategoryAllChecked(Node^.Index);
end;

function TVSTCategoryCheckTable.IsAllUncheckedCategory(Node: PVirtualNode): Boolean;
begin
  Result:= False;
  if not Assigned(Node) then Exit;
  if not FTree.GetNodeLevel(Node)=0 then Exit;
  Result:= IsCategoryAllUnchecked(Node^.Index);
end;



procedure TVSTCategoryCheckTable.CheckAll(const AChecked: Boolean);
var
  Node: PVirtualNode;
begin
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if FTree.GetNodeLevel(Node)=0 then
      CheckCategory(Node, AChecked);
    Node:= FTree.GetNext(Node);
  end;
  FTree.Refresh;
end;

procedure TVSTCategoryCheckTable.CheckCategory(Node: PVirtualNode;
  const AChecked: Boolean);
var
  Ind, i: Integer;
begin
  if not Assigned(Node) then Exit;
  if FTree.GetNodeLevel(Node)<>0 then Exit;
  Ind:= Node^.Index;
  if MIsNil(FSelected) then Exit;
  if VIsNil(FSelected[Ind]) then Exit;
  for i:= 0 to High(FSelected[Ind]) do
  begin
    if AChecked then
    begin
      Check(Ind, i);
      Node^.CheckState:= csCheckedNormal
    end
    else begin
      Uncheck(Ind, i);
      Node^.CheckState:= csUnCheckedNormal;
    end;
  end;
end;

procedure TVSTCategoryCheckTable.SetCategoryCheckState(Node: PVirtualNode);
begin
  if IsAllCheckedCategory(Node) then
   Node^.CheckState:= csCheckedNormal
  else if IsAllUncheckedCategory(Node) then
    Node^.CheckState:= csUncheckedNormal
  else if IsHasCheckedCategory(Node) then
    Node^.CheckState:= csMixedNormal;
end;

procedure TVSTCategoryCheckTable.CheckCategory(const AIndex: Integer; const AChecked: Boolean);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(AIndex);
  if not Assigned(Node) then Exit;
  CheckCategory(Node, AChecked);
end;

{ TVSTCategoryRadioButtonTable }

function TVSTCategoryRadioButtonTable.GetSelectedIndex1: Integer;
var
  Ind1, Ind2: Integer;
begin
  SelectedIndexes(Ind1, Ind2);
  Result:= Ind1;
end;

function TVSTCategoryRadioButtonTable.GetSelectedIndex2: Integer;
var
  Ind1, Ind2: Integer;
begin
  SelectedIndexes(Ind1, Ind2);
  Result:= Ind2;
end;

procedure TVSTCategoryRadioButtonTable.InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if FTree.GetNodeLevel(Node)=1 then
  begin
    Node^.CheckType:= ctRadioButton;
    Node^.CheckState:= csUncheckedNormal;
  end;
end;

procedure TVSTCategoryRadioButtonTable.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  if not FCanSelect then Exit;

  Node:= FTree.GetNodeAt(X, Y);
  if not Assigned(Node) then Exit;

  if Button=mbRight then
  begin
    if FCanUnselect then
      UnselectNode;
  end
  else if Button=mbLeft then
  begin
    if FTree.GetNodeLevel(Node)=1 then
      SelectNode(Node);
  end;
end;

procedure TVSTCategoryRadioButtonTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then UnselectNode;
  inherited SetCanSelect(AValue);
end;

procedure TVSTCategoryRadioButtonTable.SelectNode(Node: PVirtualNode);
var
  SelectedNode: PVirtualNode;
  i, j: Integer;
begin
  //unselect
  if IsSelected then
  begin
    SelectedIndexes(i,j);
    SelectedNode:= NodeFromIndex(i,j);
    SelectedNode^.CheckState:= csUncheckedNormal;
    FSelected[i,j]:= False;
  end;
  //select
  if Assigned(Node) then
  begin
    i:= (Node^.Parent)^.Index;
    j:= Node^.Index;
    FSelected[i, j]:= True;
    FTree.FocusedNode:= Node;
    Node^.CheckState:= csCheckedNormal;
  end;

  if Assigned(FOnSelect) then FOnSelect;

  FTree.Refresh;
end;

procedure TVSTCategoryRadioButtonTable.UnselectNode;
begin
  SelectNode(nil);
end;

constructor TVSTCategoryRadioButtonTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.OnInitNode:= @InitNode;
  FTree.OnMouseDown:= @MouseDown;
end;

destructor TVSTCategoryRadioButtonTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTCategoryRadioButtonTable.Draw;
begin
  UnselectNode;
  inherited Draw;
  FTree.Refresh;
end;

procedure TVSTCategoryRadioButtonTable.ValuesClear;
begin
  UnselectNode;
  inherited ValuesClear;
end;

procedure TVSTCategoryRadioButtonTable.Select(const AIndex1, AIndex2: Integer);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(AIndex1, AIndex2);
  if Assigned(Node) then SelectNode(Node);
end;

procedure TVSTCategoryRadioButtonTable.SelectedIndexes(out AIndex1, AIndex2: Integer);
begin
  MIndexOf(FSelected, True, AIndex1, AIndex2);
end;

{ TVSTCustomCategoryTable }

procedure TVSTCustomCategoryTable.SetTreeLinesVisible(AValue: Boolean);
begin
  if Avalue then
    FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions + [toShowTreeLines]
  else
    FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
end;

procedure TVSTCustomCategoryTable.HeaderClear;
begin
  inherited HeaderClear;
  FDataValues:= nil;
end;

procedure TVSTCustomCategoryTable.GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  i, j, k: Integer;
begin
  CellText:= EmptyStr;
  if FTree.GetNodeLevel(Node)=0 then
  begin
    if Column=0 then
    begin
      i:= Node^.Index;
      CellText:= FCategoryValues[i];
    end;
  end
  else begin
    if Column>=0 then
    begin
      i:= Column;
      j:= (Node^.Parent)^.Index;
      k:= Node^.Index;
      CellText:= FDataValues[i,j,k];
    end;
  end;
end;

function TVSTCustomCategoryTable.IsIndexesCorrect(const AIndex1,AIndex2: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FDataValues) then Exit;
  if MIsNil(FDataValues[0]) then Exit;
  if (AIndex1>=0) and (AIndex1<=High(FDataValues[0])) then
  begin
    if VIsNil(FDataValues[0,AIndex1]) then Exit;
    Result:= (AIndex2>=0) and (AIndex2<=High(FDataValues[0,AIndex1]))
  end;
end;

function TVSTCustomCategoryTable.IsCellSelected(Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  Result:= False;
  if FTree.GetNodeLevel(Node)<>1 then Exit;
  Result:= FSelected[(Node^.Parent)^.Index, Node^.Index];
end;

function TVSTCustomCategoryTable.GetIsSelected: Boolean;
var
  Ind1, Ind2: Integer;
begin
  Result:= False;
  if not Assigned(Self) then Exit;
  MIndexOf(FSelected, True, Ind1, Ind2);
  Result:= (Ind1>=0) and (Ind2>=0);
end;

constructor TVSTCustomCategoryTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  Clear;
  FTree.Margin:= 0;
  FCanSelect:= True;
  FSelectedBGColor:= FTree.Color;
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toCheckSupport];
  FTree.TreeOptions.AutoOptions:= FTree.TreeOptions.AutoOptions + [toAutoSpanColumns];
  FTree.LineStyle:= lsSolid;
  FTree.OnGetText:= @GetText;
end;

destructor TVSTCustomCategoryTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTCustomCategoryTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
begin
  inherited AddColumn(ACaption, AWidth, ACaptionAlignment);
  MAppend(FDataValues, nil);
end;

procedure TVSTCustomCategoryTable.SetColumn(const AColIndex: Integer;
  const AValues: TStrMatrix; const AValuesAlignment: TAlignment);
begin
  FDataValues[AColIndex]:= MCut(AValues);
  FTree.Header.Columns[AColIndex].Alignment:= AValuesAlignment;
end;

procedure TVSTCustomCategoryTable.SetColumn(const ACaption: String;
  const AValues: TStrMatrix; const AValuesAlignment: TAlignment);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColIndex>=0 then
    SetColumn(ColIndex, AValues, AValuesAlignment);
end;

procedure TVSTCustomCategoryTable.SetCategories(const AValues: TStrVector);
begin
  FCategoryValues:= VCut(AValues);
end;

procedure TVSTCustomCategoryTable.Clear;
begin
  ValuesClear;
  HeaderClear;
end;

procedure TVSTCustomCategoryTable.ValuesClear;
var
  i: Integer;
begin
  FCategoryValues:= nil;
  FSelected:= nil;
  for i:=0 to High(FDataValues) do
    FDataValues[i]:= nil;
  inherited ValuesClear;
end;

procedure TVSTCustomCategoryTable.Draw;
var
  ColIndex, CategoryIndex, MaxLength: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;
  if MIsNil(FDataValues) then Exit;

  MaxLength:= 0;
  for ColIndex:= 0 to High(FHeaderCaptions) do
  begin
    if Length(FDataValues[ColIndex])>MaxLength then
      MaxLength:= Length(FDataValues[ColIndex]);
  end;
  for ColIndex:= 0 to High(FHeaderCaptions) do
    MReDim(FDataValues[ColIndex], MaxLength);

  VReDim(FCategoryValues, MaxLength, EmptyStr);

  for CategoryIndex:= 0 to High(FCategoryValues) do
  begin
    MaxLength:= 0;
    for ColIndex:= 0 to High(FHeaderCaptions) do
    begin
      if Length(FDataValues[ColIndex, CategoryIndex])>MaxLength then
        MaxLength:= Length(FDataValues[ColIndex, CategoryIndex]);
    end;
    for ColIndex:= 0 to High(FHeaderCaptions) do
      VReDim(FDataValues[ColIndex, CategoryIndex], MaxLength);
  end;

  MReDim(FSelected, Length(FCategoryValues));
  for CategoryIndex:= 0 to High(FCategoryValues) do
    VReDim(FSelected[CategoryIndex], Length(FDataValues[0,CategoryIndex]));

  VSTLoad(FTree, FDataValues[0], False);

  SetColumnWidths;

end;

procedure TVSTCustomCategoryTable.ExpandAll(const AExpand: Boolean);
var
  Node: PVirtualNode;
begin
  if MIsNil(FDataValues) then Exit;
  Node:= FTree.GetFirst;
  while Assigned(Node) do
  begin
    if FTree.GetNodeLevel(Node)=0 then
      FTree.Expanded[Node]:= AExpand;
    Node:= FTree.GetNext(Node);
  end;
end;

procedure TVSTCustomCategoryTable.Show(const AIndex1, AIndex2: Integer);
var
  Node: PVirtualNode;
begin
  if not IsIndexesCorrect(AIndex1, AIndex2) then Exit;
  Node:= NodeFromIndex(AIndex1, AIndex2);
  if not Assigned(Node) then Exit;
  FTree.Expanded[Node^.Parent]:= True;
  FTree.FocusedNode:= Node;
end;

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

function TVSTCoreTable.GetVisible: Boolean;
begin
  Result:= FTree.Visible;
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

  //function CalcWidth(const AWidth: Integer): Integer;
  //begin
  //  Result:= Round(AWidth*ScreenInfo.PixelsPerInchX/96);
  //end;

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
    FTree.Header.Columns[i].Width:= FColumnWidths[i];//CalcWidth(FColumnWidths[i]);
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

procedure TVSTCoreTable.SetDesignTimePPI;
begin
  FDesignTimePPI:= ControlDesignTimePPI(FTree.Parent);
end;

procedure TVSTCoreTable.SetDefaultHeights(const AHeaderHeight, ARowHeight: Integer);
var
  H: Integer;
begin
  H:= SizeFromDesignTimeToDefault(FTree.DefaultNodeHeight, FDesignTimePPI);
  if H<ARowHeight then
    SetRowHeight(ARowHeight);
  H:= SizeFromDesignTimeToDefault(FTree.Header.DefaultHeight, FDesignTimePPI);
  if H<AHeaderHeight then
    SetHeaderHeight(AHeaderHeight);
end;

constructor TVSTCoreTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  FTree:= ATree;

  SetDesignTimePPI;

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

  FCanUnselect:= True;

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
    FTree.Header.Height := Round(FTree.Header.DefaultHeight * ZoomFactor);
    for i := 0 to High(FColumnWidths) do
      FTree.Header.Columns.Items[i].Width:= Round(FColumnWidths[i] * ZoomFactor);
    VSTNodeHeights(FTree, Round(FTree.DefaultNodeHeight * ZoomFactor));
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
  W:= WidthFromDefaultToScreen(AWidth);
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
  FTree.DefaultNodeHeight:= SizeFromDefaultToDesignTime(AHeight, FDesignTimePPI);
  VSTNodeHeights(FTree, FTree.DefaultNodeHeight);
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetHeaderHeight(const AHeight: Integer);
begin
  FTree.Header.DefaultHeight:= SizeFromDefaultToDesignTime(AHeight, FDesignTimePPI);
  FTree.Header.Height:= FTree.Header.DefaultHeight;
  FTree.Refresh;
end;

procedure TVSTCoreTable.SetAllHeight(const AHeight: Integer);
begin
  SetRowHeight(AHeight);
  SetHeaderHeight(AHeight);
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
  NodeCount, HeaderHeight, NodeHeight: Integer;
begin
  HeaderHeight:= 0;
  if FHeaderVisible then
    HeaderHeight:= HeightFromScreenToDesignTime(FTree.Header.Height, FDesignTimePPI);
  NodeHeight:= HeightFromScreenToDesignTime(FTree.DefaultNodeHeight, FDesignTimePPI);
  NodeCount:= MMaxLength(FDataValues);
  if NodeCount=0 then
    NodeCount:= 1;
  if (MaxAutoHeightRowCount>0) and (NodeCount>MaxAutoHeightRowCount) then
    NodeCount:= MaxAutoHeightRowCount;
  Result:= HeaderHeight + NodeCount*NodeHeight;
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

{ TVSTCheckTable }

function TVSTCheckTable.GetIsAllUnchecked: Boolean;
begin
  Result:= VIsAllFalse(FSelected);
end;

procedure TVSTCheckTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then CheckAll(False);
  inherited SetCanSelect(AValue);
end;

function TVSTCheckTable.GetIsAllChecked: Boolean;
begin
  Result:= VIsAllTrue(FSelected);
end;

function TVSTCheckTable.GetChecked(AIndex: Integer): Boolean;
begin
  Result:= False;
  if not IsIndexCorrect(AIndex) then Exit;
  Result:= FSelected[AIndex];
end;

procedure TVSTCheckTable.SetChecked(AIndex: Integer; AValue: Boolean);
begin
  if not IsIndexCorrect(AIndex) then Exit;
  if Avalue then
    Check(AIndex)
  else
    Uncheck(AIndex);
end;

procedure TVSTCheckTable.ReverseCheckState(Node: PVirtualNode);
begin
  if Node^.CheckState=csUncheckedNormal then
    Check(Node)
  else if Node^.CheckState=csCheckedNormal then
    Uncheck(Node);
end;

procedure TVSTCheckTable.CheckNode(Node: PVirtualNode; const AChecked: Boolean);
begin
  if (not Assigned(Node)) or VIsNil(FSelected) then Exit;

  if FMaxCheckedCount>=0 then
    if AChecked and (CheckedCount=FMaxCheckedCount) then
      Exit;

  if AChecked then
    Node^.CheckState:= csCheckedNormal
  else
    Node^.CheckState:= csUnCheckedNormal;
  FSelected[Node^.Index]:= AChecked;

  if Assigned(FOnCheck) then
    FOnCheck(Node^.Index, AChecked);
  if Assigned(FOnSelect) then
    FOnSelect;

  FTree.Refresh;
end;

procedure TVSTCheckTable.Check(Node: PVirtualNode);
begin
  CheckNode(Node, True);
end;

procedure TVSTCheckTable.Uncheck(Node: PVirtualNode);
begin
  CheckNode(Node, False);
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

function TVSTCheckTable.GetSelected: TBoolVector;
begin
  Result:= VCut(FSelected);
end;

function TVSTCheckTable.GetCheckedCount: Integer;
begin
  Result:= VCountIf(FSelected, True);
end;

function TVSTCheckTable.GetUncheckedCount: Integer;
begin
  Result:= VCountIf(FSelected, False);
end;

procedure TVSTCheckTable.MaxCheckedCountClear;
begin
  FMaxCheckedCount:= -1;
end;

procedure TVSTCheckTable.SetMaxCheckedCount(AValue: Integer);
var
  i, Count, Delta, N: Integer;
begin
  if FMaxCheckedCount=AValue then Exit;
  FMaxCheckedCount:= AValue;

  if FMaxCheckedCount<0 then Exit;

  Count:= CheckedCount;
  if Count<=FMaxCheckedCount then Exit;

  Delta:= Count - FMaxCheckedCount;
  N:= 0;
  for i:= High(FSelected) downto 0 do
  begin
    if FSelected[i] then
    begin
      Uncheck(i);
      N:= N + 1;
    end;
    if N=Delta then break;
  end;

end;

procedure TVSTCheckTable.SetSelected(AValue: TBoolVector);
var
  i: Integer;
begin
  for i:= 0 to High(AValue) do
  begin
    if AValue[i] then
      Check(i)
    else
      Uncheck(i);
  end;
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
  begin
    if FCanUnselect then
      CheckAll(False)
  end
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

constructor TVSTCheckTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toCheckSupport];

  FMaxCheckedCount:= -1;

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
  FSelected:= nil;
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

{ TVSTSimpleTable }

procedure TVSTSimpleTable.SetColumnVisibles(AValue: TBoolVector);
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

function TVSTSimpleTable.GetColCount: Integer;
begin
  Result:= Length(FHeaderCaptions);
end;

function TVSTSimpleTable.GetRowCount: Integer;
begin
  Result:= 0;
  if not MIsNil(FDataValues) then
    Result:= Length(FDataValues[0]);
end;

function TVSTSimpleTable.GetIsSelected: Boolean;
begin
  Result:= False;
  if not Assigned(Self) then Exit;
  Result:= VIsTrue(FSelected);
end;

function TVSTSimpleTable.IsCellSelected(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result:= inherited IsCellSelected(Node, Column);
  if not Result then Exit;
  Result:= FSelected[Node^.Index];
end;

procedure TVSTSimpleTable.HeaderClear;
begin
  inherited HeaderClear;
  FDataValues:= nil;
  FColumnVisibles:= nil;
end;

procedure TVSTSimpleTable.GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
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

function TVSTSimpleTable.IsIndexCorrect(const AIndex: Integer): Boolean;
begin
  Result:= False;
  if MIsNil(FDataValues) then Exit;
  if VIsNil(FDataValues[0]) then Exit;
  Result:= (AIndex>=0) and (AIndex<=High(FDataValues[0]));
end;

constructor TVSTSimpleTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.TreeOptions.PaintOptions:= FTree.TreeOptions.PaintOptions - [toShowTreeLines];
  FTree.OnGetText:= @GetText;
end;

destructor TVSTSimpleTable.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTSimpleTable.ValuesClear;
begin
  FSelected:= nil;
  inherited ValuesClear;
end;

procedure TVSTSimpleTable.Draw;
var
  i, MaxLength: Integer;
begin
  FTree.Clear;
  if VIsNil(FHeaderCaptions) then Exit;

  MaxLength:= MMaxLength(FDataValues);
  VDim(FSelected, MaxLength, False);
  for i:= 0 to High(FDataValues) do
    if Length(FDataValues[i])<MaxLength then
      VReDim(FDataValues[i], MaxLength, EmptyStr);

  VSTLoad(FTree, FDataValues[0]);

  inherited Draw;
end;

procedure TVSTSimpleTable.AddColumn(const ACaption: String;
  const AWidth: Integer; const ACaptionAlignment: TAlignment);
begin
  inherited AddColumn(ACaption, AWidth, ACaptionAlignment);
  MAppend(FDataValues, nil);
  VAppend(FColumnVisibles, True);
end;

procedure TVSTSimpleTable.SetColumn(const AColIndex: Integer;
  const AValues: TStrVector; const AValuesAlignment: TAlignment);
begin
  FDataValues[AColIndex]:= VCut(AValues);
  FTree.Header.Columns[AColIndex].Alignment:= AValuesAlignment;
end;

procedure TVSTSimpleTable.SetColumn(const ACaption: String;
  const AValues: TStrVector; const AValuesAlignment: TAlignment);
var
  ColIndex: Integer;
begin
  ColIndex:= VIndexOf(FHeaderCaptions, ACaption);
  if ColIndex>=0 then
    SetColumn(ColIndex, AValues, AValuesAlignment);
end;

procedure TVSTSimpleTable.Show(const AIndex: Integer);
var
  Node: PVirtualNode;
begin
  if not IsIndexCorrect(AIndex) then Exit;
  Node:= NodeFromIndex(AIndex);
  if not Assigned(Node) then Exit;
  FTree.FocusedNode:= Node;
end;

{ TVSTTable }

procedure TVSTTable.Select(const AIndex: Integer);
var
  Node: PVirtualNode;
begin
  Node:= NodeFromIndex(AIndex);
  if Assigned(Node) then SelectNode(Node);
end;

procedure TVSTTable.Select(const AColIndex: Integer; const AValue: String);
var
  Ind: Integer;
begin
  Ind:= VIndexOf(FDataValues[AColIndex], AValue);
  if Ind>=0 then
    Select(Ind);
end;

procedure TVSTTable.Select(const AColumnCaption, AValue: String);
var
  Ind: Integer;
begin
  Ind:= VIndexOf(FHeaderCaptions, AColumnCaption);
  if Ind>=0 then
    Select(Ind, AValue);
end;

function TVSTTable.ReSelect(const AIDVector: TIntVector; const AIDValue: Integer;
                            const AFirstRowSelectIfNotFound: Boolean = False): Boolean;
var
  Index: Integer;
begin
  Result:= False;
  if VIsNil(AIDVector) then Exit;

  Index:= -1;
  if AIDValue<=0 then
  begin
    if AFirstRowSelectIfNotFound then
      Index:= 0;
  end
  else begin
    Index:= VIndexOf(AIDVector, AIDValue);
    if (Index<0) and AFirstRowSelectIfNotFound then
      Index:= 0;
  end;

  if Index>=0 then
  begin
    Show(Index);
    Select(Index);
    Result:= True;
  end;
end;

function TVSTTable.ReSelect(const AIDVector: TDateVector; const AIDValue: TDate;
                            const AFirstRowSelectIfNotFound: Boolean = False): Boolean;
var
  Index: Integer;
begin
  Result:= False;
  if VIsNil(AIDVector) then Exit;

  Index:= -1;
  if AIDValue<=0 then
  begin
    if AFirstRowSelectIfNotFound then
      Index:= 0;
  end
  else begin
    Index:= VIndexOfDate(AIDVector, AIDValue);
    if (Index<0) and AFirstRowSelectIfNotFound then
      Index:= 0;
  end;

  if Index>=0 then
  begin
    Show(Index);
    Select(Index);
    Result:= True;
  end;

  //Index:= -1;
  //if AIDValue>0 then
  //  Index:= VIndexOfDate(AIDVector, AIDValue);
  //if Index>=0 then
  //  Select(Index);
end;

procedure TVSTTable.Save(const AColumnTypes: TVSTColumnTypes;
                         const ASheetName: String = 'Лист1';
                         const ADoneMessage: String = 'Выполнено!';
                         const ALandscape: Boolean = False);
var
  Exporter: TSheetsExporter;
  Sheet: TsWorksheet;
  Writer: TSheetWriter;
  VisibleColumnWidths: TIntVector;

  function AlignmentConvert(const AAlignment: TAlignment): TsHorAlignment;
  begin
    Result:= haDefault;
    if AAlignment = taLeftJustify then
      Result:= haLeft
    else if AAlignment = taRightJustify then
      Result:= haRight
    else if AAlignment = taCenter then
      Result:= haCenter;
  end;

  procedure ValueWrite(const ARow, ACol: Integer; const AValue: String; const AType: TVSTColumnType);
  var
    IntValue: Integer;
    DTValue: TDateTime;
    DblValue: Double;
  begin
    if AType = ctInteger then
    begin
      if TryStrToInt(AValue, IntValue) then
        Writer.WriteNumber(ARow, ACol, IntValue, cbtOuter)
      else
        Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
    end
    else if AType = ctDate then
    begin
      if TryStrToDate(AValue, DTValue) then
        Writer.WriteDate(ARow, ACol, DTValue, cbtOuter)
      else
        Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
    end
    else if AType = ctTime then
    begin
      if TryStrToTime(AValue, DTValue) then
        Writer.WriteTime(ARow, ACol, DTValue, cbtOuter)
      else
        Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
    end
    else if AType = ctDouble then
    begin
      if TryStrToFloat(AValue, DblValue) then
        Writer.WriteNumber(ARow, ACol, DblValue, cbtOuter)
      else
        Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
    end
    else Writer.WriteText(ARow, ACol, AValue, cbtOuter, True, True);
  end;

  procedure SheetWrite;
  var
    i, j, R, C: Integer;
  begin

    if HeaderVisible then
    begin
      Writer.SetFont(HeaderFont);
      R:= 1;
      C:= 0;
      for i:= 0 to High(FHeaderCaptions) do
      begin
        if not FColumnVisibles[i] then continue;
        C:= C + 1;
        Writer.SetBackground(FColumnHeaderBGColors[i]);
        Writer.SetAlignment(AlignmentConvert(FTree.Header.Columns[i].CaptionAlignment), vaCenter);
        Writer.WriteText(R, C, FHeaderCaptions[i], cbtOuter);
      end;
      //Writer.SetRowHeight(R, FTree.Header.DefaultHeight);
    end;

    Writer.SetFont(ValuesFont);
    C:= 0;
    for i:= 0 to High(FDataValues) do
    begin
      if not FColumnVisibles[i] then continue;
      R:= Ord(HeaderVisible);
      C:= C + 1;
      Writer.SetBackground(FColumnValuesBGColors[i]);
      Writer.SetAlignment(AlignmentConvert(FTree.Header.Columns[i].Alignment), vaCenter);
      for j:= 0 to High(FDataValues[i]) do
      begin
        R:= R + 1;
        ValueWrite(R, C, FDataValues[i,j], AColumnTypes[i]);
      end;
    end;
  end;

begin
  Exporter:= TSheetsExporter.Create;
  try
    Sheet:= Exporter.AddWorksheet(ASheetName);

    VisibleColumnWidths:= VCut(FColumnWidths, FColumnVisibles);
    Writer:= TSheetWriter.Create(VisibleColumnWidths, Sheet, nil, FTree.DefaultNodeHeight);
    try
      SheetWrite;
    finally
      FreeAndNil(Writer);
    end;

    if ALandscape then Exporter.PageSettings(spoLandscape);
    Exporter.Save(ADoneMessage);
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TVSTTable.SetAutosizeRowHeights(AValue: Boolean);
begin
  if FAutosizeRowHeights=AValue then Exit;
  FAutosizeRowHeights:= AValue;

  if FAutosizeRowHeights then
    FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions + [toVariableNodeHeight]
  else
    FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions - [toVariableNodeHeight];

  FTree.ShowHint:= not FAutosizeRowHeights;
  FTree.Refresh;
end;

procedure TVSTTable.SelectNode(Node: PVirtualNode);
var
  NewSelectedIndex: Integer;
begin
  NewSelectedIndex:= -1;
  if Assigned(Node) then
  begin
    NewSelectedIndex:= Node^.Index;
    //cancel same selection
    if NewSelectedIndex=SelectedIndex then Exit;
  end;

  //unselect
  if IsSelected then
    FSelected[SelectedIndex]:= False;
  //select
  if Assigned(Node) then
  begin
    FSelected[NewSelectedIndex]:= True;
    FTree.FocusedNode:= Node;
  end;

  if Assigned(FOnSelect) then FOnSelect;
  FTree.Refresh;
end;

procedure TVSTTable.UnselectNode;
begin
  SelectNode(nil);
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
  begin
    if FCanUnselect then
      UnselectNode;
  end
  else if Button=mbLeft then
    SelectNode(Node);
end;

procedure TVSTTable.MoveSelection(const ADeltaIndex: Integer);
var
  Ind: Integer;
begin
  if (not IsSelected) or (ADeltaIndex=0) then Exit;

  Ind:= SelectedIndex+ADeltaIndex;
  if not IsIndexCorrect(Ind) then Exit;

  Select(Ind);

end;

procedure TVSTTable.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_DELETE then
  begin
    if Assigned(FOnDelKeyDown) then
      FOnDelKeyDown
  end
  else if Key=VK_RETURN then
  begin
    if Assigned(FOnReturnKeyDown) then
      FOnReturnKeyDown
  end
  else if Key=VK_UP then
    MoveSelection(-1)
  else if Key=VK_DOWN then
    MoveSelection(1);
end;

procedure TVSTTable.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if FAutosizeRowHeights then
  begin
    InitialStates:= InitialStates + [ivsMultiline];
    Node^.States:= Node^.States + [vsMultiline];
  end
  else begin
    InitialStates:= InitialStates - [ivsMultiline];
    Node^.States:= Node^.States - [vsMultiline];
  end;
end;

procedure TVSTTable.MeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
var
  Height, i: Integer;
begin
  NodeHeight:= FTree.DefaultNodeHeight;
  if not FAutosizeRowHeights then Exit;

  FTree.Font.Assign(FValuesFont);
  for i:=0 to High(FHeaderCaptions) do
  begin
    Height:= FTree.ComputeNodeHeight(TargetCanvas, Node, i);
    if Height>NodeHeight then
      NodeHeight:= Height;
  end;
end;

procedure TVSTTable.SetCanSelect(AValue: Boolean);
begin
  if not AValue then UnselectNode;
  inherited SetCanSelect(AValue);
end;

function TVSTTable.GetSelectedIndex: Integer;
begin
  Result:= VIndexOf(FSelected, True);
end;

constructor TVSTTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);
  FTree.Margin:= 0;
  FTree.TreeOptions.MiscOptions:= FTree.TreeOptions.MiscOptions - [toCheckSupport];
  FTree.OnMouseDown:= @MouseDown;
  FTree.OnKeyDown:= @KeyDown;
  FTree.OnInitNode:= @InitNode;
  FTree.OnMeasureItem:= @MeasureItem;
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

procedure TVSTTable.UnSelect;
begin
  UnselectNode;
end;



end.

