module Fable.Inferno

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

module Props =
    type ICSSProp =
        interface end

    type CSSProp =
        | BoxFlex of float
        | BoxFlexGroup of float
        | ColumnCount of float
        | Cursor of string
        | Flex of U2<float, string>
        | FlexGrow of float
        | FlexShrink of float
        | FontWeight of U2<float, string>
        | LineClamp of float
        | LineHeight of U2<float, string>
        | Opacity of float
        | Order of float
        | Orphans of float
        | Widows of float
        | ZIndex of float
        | Zoom of float
        | FontSize of U2<float, string>
        | FillOpacity of float
        | StrokeOpacity of float
        | StrokeWidth of float
        | AlignContent of obj
        | AlignItems of obj
        | AlignSelf of obj
        | AlignmentAdjust of obj
        | AlignmentBaseline of obj
        | AnimationDelay of obj
        | AnimationDirection of obj
        | AnimationIterationCount of obj
        | AnimationName of obj
        | AnimationPlayState of obj
        | Appearance of obj
        | BackfaceVisibility of obj
        | BackgroundBlendMode of obj
        | BackgroundColor of obj
        | BackgroundComposite of obj
        | BackgroundImage of obj
        | BackgroundOrigin of obj
        | BackgroundPositionX of obj
        | BackgroundRepeat of obj
        | BaselineShift of obj
        | Behavior of obj
        | Border of obj
        | BorderBottomLeftRadius of obj
        | BorderBottomRightRadius of obj
        | BorderBottomWidth of obj
        | BorderCollapse of obj
        | BorderColor of obj
        | BorderCornerShape of obj
        | BorderImageSource of obj
        | BorderImageWidth of obj
        | BorderLeft of obj
        | BorderLeftColor of obj
        | BorderLeftStyle of obj
        | BorderLeftWidth of obj
        | BorderRight of obj
        | BorderRightColor of obj
        | BorderRightStyle of obj
        | BorderRightWidth of obj
        | BorderSpacing of obj
        | BorderStyle of obj
        | BorderTop of obj
        | BorderTopColor of obj
        | BorderTopLeftRadius of obj
        | BorderTopRightRadius of obj
        | BorderTopStyle of obj
        | BorderTopWidth of obj
        | BorderWidth of obj
        | Bottom of obj
        | BoxAlign of obj
        | BoxDecorationBreak of obj
        | BoxDirection of obj
        | BoxLineProgression of obj
        | BoxLines of obj
        | BoxOrdinalGroup of obj
        | BreakAfter of obj
        | BreakBefore of obj
        | BreakInside of obj
        | Clear of obj
        | Clip of obj
        | ClipRule of obj
        | Color of obj
        | ColumnFill of obj
        | ColumnGap of obj
        | ColumnRule of obj
        | ColumnRuleColor of obj
        | ColumnRuleWidth of obj
        | ColumnSpan of obj
        | ColumnWidth of obj
        | Columns of obj
        | CounterIncrement of obj
        | CounterReset of obj
        | Cue of obj
        | CueAfter of obj
        | Direction of obj
        | Display of obj
        | Fill of obj
        | FillRule of obj
        | Filter of obj
        | FlexAlign of obj
        | FlexBasis of obj
        | FlexDirection of obj
        | FlexFlow of obj
        | FlexItemAlign of obj
        | FlexLinePack of obj
        | FlexOrder of obj
        | FlexWrap of obj
        | Float of obj
        | FlowFrom of obj
        | Font of obj
        | FontFamily of obj
        | FontKerning of obj
        | FontSizeAdjust of obj
        | FontStretch of obj
        | FontStyle of obj
        | FontSynthesis of obj
        | FontVariant of obj
        | FontVariantAlternates of obj
        | GridArea of obj
        | GridColumn of obj
        | GridColumnEnd of obj
        | GridColumnStart of obj
        | GridRow of obj
        | GridRowEnd of obj
        | GridRowPosition of obj
        | GridRowSpan of obj
        | GridTemplateAreas of obj
        | GridTemplateColumns of obj
        | GridTemplateRows of obj
        | Height of obj
        | HyphenateLimitChars of obj
        | HyphenateLimitLines of obj
        | HyphenateLimitZone of obj
        | Hyphens of obj
        | ImeMode of obj
        | JustifyContent of obj
        | LayoutGrid of obj
        | LayoutGridChar of obj
        | LayoutGridLine of obj
        | LayoutGridMode of obj
        | LayoutGridType of obj
        | Left of obj
        | LetterSpacing of obj
        | LineBreak of obj
        | ListStyle of obj
        | ListStyleImage of obj
        | ListStylePosition of obj
        | ListStyleType of obj
        | Margin of obj
        | MarginBottom of obj
        | MarginLeft of obj
        | MarginRight of obj
        | MarginTop of obj
        | MarqueeDirection of obj
        | MarqueeStyle of obj
        | Mask of obj
        | MaskBorder of obj
        | MaskBorderRepeat of obj
        | MaskBorderSlice of obj
        | MaskBorderSource of obj
        | MaskBorderWidth of obj
        | MaskClip of obj
        | MaskOrigin of obj
        | MaxFontSize of obj
        | MaxHeight of obj
        | MaxWidth of obj
        | MinHeight of obj
        | MinWidth of obj
        | Outline of obj
        | OutlineColor of obj
        | OutlineOffset of obj
        | Overflow of obj
        | OverflowStyle of obj
        | OverflowX of obj
        | Padding of obj
        | PaddingBottom of obj
        | PaddingLeft of obj
        | PaddingRight of obj
        | PaddingTop of obj
        | PageBreakAfter of obj
        | PageBreakBefore of obj
        | PageBreakInside of obj
        | Pause of obj
        | PauseAfter of obj
        | PauseBefore of obj
        | Perspective of obj
        | PerspectiveOrigin of obj
        | PointerEvents of obj
        | Position of obj
        | PunctuationTrim of obj
        | Quotes of obj
        | RegionFragment of obj
        | RestAfter of obj
        | RestBefore of obj
        | Right of obj
        | RubyAlign of obj
        | RubyPosition of obj
        | ShapeImageThreshold of obj
        | ShapeInside of obj
        | ShapeMargin of obj
        | ShapeOutside of obj
        | Speak of obj
        | SpeakAs of obj
        | TabSize of obj
        | TableLayout of obj
        | TextAlign of obj
        | TextAlignLast of obj
        | TextDecoration of obj
        | TextDecorationColor of obj
        | TextDecorationLine of obj
        | TextDecorationLineThrough of obj
        | TextDecorationNone of obj
        | TextDecorationOverline of obj
        | TextDecorationSkip of obj
        | TextDecorationStyle of obj
        | TextDecorationUnderline of obj
        | TextEmphasis of obj
        | TextEmphasisColor of obj
        | TextEmphasisStyle of obj
        | TextHeight of obj
        | TextIndent of obj
        | TextJustifyTrim of obj
        | TextKashidaSpace of obj
        | TextLineThrough of obj
        | TextLineThroughColor of obj
        | TextLineThroughMode of obj
        | TextLineThroughStyle of obj
        | TextLineThroughWidth of obj
        | TextOverflow of obj
        | TextOverline of obj
        | TextOverlineColor of obj
        | TextOverlineMode of obj
        | TextOverlineStyle of obj
        | TextOverlineWidth of obj
        | TextRendering of obj
        | TextScript of obj
        | TextShadow of obj
        | TextTransform of obj
        | TextUnderlinePosition of obj
        | TextUnderlineStyle of obj
        | Top of obj
        | TouchAction of obj
        | Transform of obj
        | TransformOrigin of obj
        | TransformOriginZ of obj
        | TransformStyle of obj
        | Transition of obj
        | TransitionDelay of obj
        | TransitionDuration of obj
        | TransitionProperty of obj
        | TransitionTimingFunction of obj
        | UnicodeBidi of obj
        | UnicodeRange of obj
        | UserFocus of obj
        | UserInput of obj
        | VerticalAlign of obj
        | Visibility of obj
        | VoiceBalance of obj
        | VoiceDuration of obj
        | VoiceFamily of obj
        | VoicePitch of obj
        | VoiceRange of obj
        | VoiceRate of obj
        | VoiceStress of obj
        | VoiceVolume of obj
        | WhiteSpace of obj
        | WhiteSpaceTreatment of obj
        | Width of obj
        | WordBreak of obj
        | WordSpacing of obj
        | WordWrap of obj
        | WrapFlow of obj
        | WrapMargin of obj
        | WrapOption of obj
        | WritingMode of obj
        interface ICSSProp

    type IProp =
        interface end

    type IHTMLProp =
        inherit IProp

    type Prop =
        | Key of string
        | Ref of (Browser.Element->unit)
        interface IHTMLProp

    type DOMAttr =
        | DangerouslySetInnerHTML of obj
        | OnCopy of (Browser.ClipboardEvent -> unit)
        | OnCut of (Browser.ClipboardEvent -> unit)
        | OnPaste of (Browser.ClipboardEvent -> unit)
        | OnCompositionEnd of (Browser.CompositionEvent -> unit)
        | OnCompositionStart of (Browser.CompositionEvent -> unit)
        | OnCompositionUpdate of (Browser.CompositionEvent -> unit)
        | OnFocus of (Browser.FocusEvent -> unit)
        | OnBlur of (Browser.FocusEvent -> unit)
        | OnChange of (Browser.Event -> unit)
        | OnInput of (Browser.Event -> unit)
        | OnSubmit of (Browser.Event -> unit)
        | OnLoad of (Browser.Event -> unit)
        | OnError of (Browser.Event -> unit)
        | OnKeyDown of (Browser.KeyboardEvent -> unit)
        | OnKeyPress of (Browser.KeyboardEvent -> unit)
        | OnKeyUp of (Browser.KeyboardEvent -> unit)
        | OnAbort of (Browser.Event -> unit)
        | OnCanPlay of (Browser.Event -> unit)
        | OnCanPlayThrough of (Browser.Event -> unit)
        | OnDurationChange of (Browser.Event -> unit)
        | OnEmptied of (Browser.Event -> unit)
        | OnEncrypted of (Browser.Event -> unit)
        | OnEnded of (Browser.Event -> unit)
        | OnLoadedData of (Browser.Event -> unit)
        | OnLoadedMetadata of (Browser.Event -> unit)
        | OnLoadStart of (Browser.Event -> unit)
        | OnPause of (Browser.Event -> unit)
        | OnPlay of (Browser.Event -> unit)
        | OnPlaying of (Browser.Event -> unit)
        | OnProgress of (Browser.Event -> unit)
        | OnRateChange of (Browser.Event -> unit)
        | OnSeeked of (Browser.Event -> unit)
        | OnSeeking of (Browser.Event -> unit)
        | OnStalled of (Browser.Event -> unit)
        | OnSuspend of (Browser.Event -> unit)
        | OnTimeUpdate of (Browser.Event -> unit)
        | OnVolumeChange of (Browser.Event -> unit)
        | OnWaiting of (Browser.Event -> unit)
        | OnClick of (Browser.MouseEvent -> unit)
        | OnContextMenu of (Browser.MouseEvent -> unit)
        | OnDoubleClick of (Browser.MouseEvent -> unit)
        | OnDrag of (Browser.DragEvent -> unit)
        | OnDragEnd of (Browser.DragEvent -> unit)
        | OnDragEnter of (Browser.DragEvent -> unit)
        | OnDragExit of (Browser.DragEvent -> unit)
        | OnDragLeave of (Browser.DragEvent -> unit)
        | OnDragOver of (Browser.DragEvent -> unit)
        | OnDragStart of (Browser.DragEvent -> unit)
        | OnDrop of (Browser.DragEvent -> unit)
        | OnMouseDown of (Browser.MouseEvent -> unit)
        | OnMouseEnter of (Browser.MouseEvent -> unit)
        | OnMouseLeave of (Browser.MouseEvent -> unit)
        | OnMouseMove of (Browser.MouseEvent -> unit)
        | OnMouseOut of (Browser.MouseEvent -> unit)
        | OnMouseOver of (Browser.MouseEvent -> unit)
        | OnMouseUp of (Browser.MouseEvent -> unit)
        | OnSelect of (Browser.Event -> unit)
        | OnTouchCancel of (Browser.TouchEvent -> unit)
        | OnTouchEnd of (Browser.TouchEvent -> unit)
        | OnTouchMove of (Browser.TouchEvent -> unit)
        | OnTouchStart of (Browser.TouchEvent -> unit)
        | OnScroll of (Browser.UIEvent -> unit)
        | OnWheel of (Browser.WheelEvent -> unit)
        interface IHTMLProp

    type HTMLAttr =
        | DefaultChecked of bool
        | DefaultValue of U2<string, ResizeArray<string>>
        | Accept of string
        | AcceptCharset of string
        | AccessKey of string
        | Action of string
        | AllowFullScreen of bool
        | AllowTransparency of bool
        | Alt of string
        | [<CompiledName("aria-haspopup")>] AriaHasPopup of bool
        | [<CompiledName("aria-expanded")>] AriaExpanded of bool
        | Async of bool
        | AutoComplete of string
        | AutoFocus of bool
        | AutoPlay of bool
        | Capture of bool
        | CellPadding of U2<float, string>
        | CellSpacing of U2<float, string>
        | CharSet of string
        | Challenge of string
        | Checked of bool
        | ClassID of string
        | ClassName of string
        | Cols of float
        | ColSpan of float
        | Content of string
        | ContentEditable of bool
        | ContextMenu of string
        | Controls of bool
        | Coords of string
        | CrossOrigin of string
        | Data of string
        | [<CompiledName("data-toggle")>] DataToggle of string
        | DateTime of string
        | Default of bool
        | Defer of bool
        | Dir of string
        | Disabled of bool
        | Download of obj
        | Draggable of bool
        | EncType of string
        | Form of string
        | FormAction of string
        | FormEncType of string
        | FormMethod of string
        | FormNoValidate of bool
        | FormTarget of string
        | FrameBorder of U2<float, string>
        | Headers of string
        | Height of U2<float, string>
        | Hidden of bool
        | High of float
        | Href of string
        | HrefLang of string
        | HtmlFor of string
        | HttpEquiv of string
        | Icon of string
        | Id of string
        | InputMode of string
        | Integrity of string
        | Is of string
        | KeyParams of string
        | KeyType of string
        | Kind of string
        | Label of string
        | Lang of string
        | List of string
        | Loop of bool
        | Low of float
        | Manifest of string
        | MarginHeight of float
        | MarginWidth of float
        | Max of U2<float, string>
        | MaxLength of float
        | Media of string
        | MediaGroup of string
        | Method of string
        | Min of U2<float, string>
        | MinLength of float
        | Multiple of bool
        | Muted of bool
        | Name of string
        | NoValidate of bool
        | Open of bool
        | Optimum of float
        | Pattern of string
        | Placeholder of string
        | Poster of string
        | Preload of string
        | RadioGroup of string
        | ReadOnly of bool
        | Rel of string
        | Required of bool
        | Role of string
        | Rows of float
        | RowSpan of float
        | Sandbox of string
        | Scope of string
        | Scoped of bool
        | Scrolling of string
        | Seamless of bool
        | Selected of bool
        | Shape of string
        | Size of float
        | Sizes of string
        | Span of float
        | SpellCheck of bool
        | Src of string
        | SrcDoc of string
        | SrcLang of string
        | SrcSet of string
        | Start of float
        | Step of U2<float, string>
        | Summary of string
        | TabIndex of float
        | Target of string
        | Title of string
        | Type of string
        | UseMap of string
        | Value of U2<string, ResizeArray<string>>
        | Width of U2<float, string>
        | Wmode of string
        | Wrap of string
        | About of string
        | Datatype of string
        | Inlist of obj
        | Prefix of string
        | Property of string
        | Resource of string
        | Typeof of string
        | Vocab of string
        | AutoCapitalize of string
        | AutoCorrect of string
        | AutoSave of string
        // | Color of string // Conflicts with CSSProp, shouldn't be used in HTML5
        | ItemProp of string
        | ItemScope of bool
        | ItemType of string
        | ItemID of string
        | ItemRef of string
        | Results of float
        | Security of string
        | Unselectable of bool
        interface IHTMLProp

    let inline Style (css: ICSSProp list): HTMLAttr =
        !!("style", keyValueList CaseRules.LowerFirst css)

    type SVGAttr =
        | ClipPath of string
        | Cx of U2<float, string>
        | Cy of U2<float, string>
        | D of string
        | Dx of U2<float, string>
        | Dy of U2<float, string>
        | Fill of string
        | FillOpacity of U2<float, string>
        | FontFamily of string
        | FontSize of U2<float, string>
        | Fx of U2<float, string>
        | Fy of U2<float, string>
        | GradientTransform of string
        | GradientUnits of string
        | MarkerEnd of string
        | MarkerMid of string
        | MarkerStart of string
        | Offset of U2<float, string>
        | Opacity of U2<float, string>
        | PatternContentUnits of string
        | PatternUnits of string
        | Points of string
        | PreserveAspectRatio of string
        | R of U2<float, string>
        | Rx of U2<float, string>
        | Ry of U2<float, string>
        | SpreadMethod of string
        | StopColor of string
        | StopOpacity of U2<float, string>
        | Stroke of string
        | StrokeDasharray of string
        | StrokeLinecap of string
        | StrokeMiterlimit of string
        | StrokeOpacity of U2<float, string>
        | StrokeWidth of U2<float, string>
        | TextAnchor of string
        | Transform of string
        | Version of string
        | ViewBox of string
        | Width of U2<float, string>
        | X1 of U2<float, string>
        | X2 of U2<float, string>
        | X of U2<float, string>
        | XlinkActuate of string
        | XlinkArcrole of string
        | XlinkHref of string
        | XlinkRole of string
        | XlinkShow of string
        | XlinkTitle of string
        | XlinkType of string
        | XmlBase of string
        | XmlLang of string
        | XmlSpace of string
        | Y1 of U2<float, string>
        | Y2 of U2<float, string>
        | Y of U2<float, string>
        interface IProp

open Props
open Fable.Core.JsInterop

type InfernoElement =
    interface end

[<AbstractClass; Import("default", "inferno-component")>]
type Component<[<Pojo>]'P, [<Pojo>]'S>(props: 'P) =
    [<Emit("$0.props")>]
    member __.props: 'P = jsNative
    [<Emit("$0.props.children")>]
    member __.children: InfernoElement array = jsNative
    [<Emit("$0.state")>]
    member __.state: 'S = jsNative
    [<Emit("$0.setState($1)")>]
    /// Within the constructor, use `setInitState`
    member __.setState(value: 'S): unit = jsNative
    [<Emit("this.state = $1")>]
    /// This method can only be called in the constructor
    member __.setInitState(value: 'S): unit = jsNative
    [<Emit("$0.forceUpdate($1)")>]
    member __.forceUpdate(?callBack: unit->unit): unit = jsNative
    [<Emit("$0.isMounted()")>]
    member __.isMounted(): bool = jsNative
    interface InfernoElement

[<Import("default", from="inferno-create-element")>]
let __createElFn: obj = null

[<Import("render", "inferno")>]
let render(element: InfernoElement, container: Browser.Element): unit = jsNative

let inline createElement(typ: obj, props: obj, children: InfernoElement list) =
    applySpread __createElFn (typ, props, children)

/// Instantiate a Inferno component from a type inheriting Inferno.Component<>
let inline com<'T,[<Pojo>]'P,[<Pojo>]'S when 'T :> Component<'P,'S>> (props: 'P) (children: InfernoElement list): InfernoElement =
    createElement(typedefof<'T>, props, children)

/// Instantiate a stateless component from a function
let inline fn<[<Pojo>]'P> (f: 'P -> InfernoElement) (props: 'P) (children: InfernoElement list): InfernoElement =
    createElement(f, props, children)

/// Instantiate an imported Inferno component
// let inline from<[<Pojo>]'P> (com: ComponentClass<'P>) (props: 'P) (children: InfernoElement list): InfernoElement =
//     createElement(com, props, children)

/// Instantiate a DOM Inferno element
let inline domEl (tag: string) (props: IHTMLProp list) (children: InfernoElement list): InfernoElement =
    createElement(tag, keyValueList CaseRules.LowerFirst props, children)

/// Instantiate a DOM Inferno element (void)
let inline voidEl (tag: string) (props: IHTMLProp list) : InfernoElement =
    createElement(tag, keyValueList CaseRules.LowerFirst props, [])

/// Instantiate an SVG Inferno element
let inline svgEl (tag: string) (props: #IProp list) (children: InfernoElement list): InfernoElement =
    createElement(tag, keyValueList CaseRules.LowerFirst props, children)

// Standard element
let inline a b c = domEl "a" b c
let inline abbr b c = domEl "abbr" b c
let inline address b c = domEl "address" b c
let inline article b c = domEl "article" b c
let inline aside b c = domEl "aside" b c
let inline audio b c = domEl "audio" b c
let inline b b' c = domEl "b" b' c
let inline bdi b c = domEl "bdi" b c
let inline bdo b c = domEl "bdo" b c
let inline big b c = domEl "big" b c
let inline blockquote b c = domEl "blockquote" b c
let inline body b c = domEl "body" b c
let inline button b c = domEl "button" b c
let inline canvas b c = domEl "canvas" b c
let inline caption b c = domEl "caption" b c
let inline cite b c = domEl "cite" b c
let inline code b c = domEl "code" b c
let inline colgroup b c = domEl "colgroup" b c
let inline data b c = domEl "data" b c
let inline datalist b c = domEl "datalist" b c
let inline dd b c = domEl "dd" b c
let inline del b c = domEl "del" b c
let inline details b c = domEl "details" b c
let inline dfn b c = domEl "dfn" b c
let inline dialog b c = domEl "dialog" b c
let inline div b c = domEl "div" b c
let inline dl b c = domEl "dl" b c
let inline dt b c = domEl "dt" b c
let inline em b c = domEl "em" b c
let inline fieldset b c = domEl "fieldset" b c
let inline figcaption b c = domEl "figcaption" b c
let inline figure b c = domEl "figure" b c
let inline footer b c = domEl "footer" b c
let inline form b c = domEl "form" b c
let inline h1 b c = domEl "h1" b c
let inline h2 b c = domEl "h2" b c
let inline h3 b c = domEl "h3" b c
let inline h4 b c = domEl "h4" b c
let inline h5 b c = domEl "h5" b c
let inline h6 b c = domEl "h6" b c
let inline head b c = domEl "head" b c
let inline header b c = domEl "header" b c
let inline hgroup b c = domEl "hgroup" b c
let inline html b c = domEl "html" b c
let inline i b c = domEl "i" b c
let inline iframe b c = domEl "iframe" b c
let inline ins b c = domEl "ins" b c
let inline kbd b c = domEl "kbd" b c
let inline label b c = domEl "label" b c
let inline legend b c = domEl "legend" b c
let inline li b c = domEl "li" b c
let inline main b c = domEl "main" b c
let inline map b c = domEl "map" b c
let inline mark b c = domEl "mark" b c
let inline menu b c = domEl "menu" b c
let inline meter b c = domEl "meter" b c
let inline nav b c = domEl "nav" b c
let inline noscript b c = domEl "noscript" b c
let inline ``object`` b c = domEl "object" b c
let inline ol b c = domEl "ol" b c
let inline optgroup b c = domEl "optgroup" b c
let inline option b c = domEl "option" b c
let inline output b c = domEl "output" b c
let inline p b c = domEl "p" b c
let inline picture b c = domEl "picture" b c
let inline pre b c = domEl "pre" b c
let inline progress b c = domEl "progress" b c
let inline q b c = domEl "q" b c
let inline rp b c = domEl "rp" b c
let inline rt b c = domEl "rt" b c
let inline ruby b c = domEl "ruby" b c
let inline s b c = domEl "s" b c
let inline samp b c = domEl "samp" b c
let inline script b c = domEl "script" b c
let inline section b c = domEl "section" b c
let inline select b c = domEl "select" b c
let inline small b c = domEl "small" b c
let inline span b c = domEl "span" b c
let inline strong b c = domEl "strong" b c
let inline style b c = domEl "style" b c
let inline sub b c = domEl "sub" b c
let inline summary b c = domEl "summary" b c
let inline sup b c = domEl "sup" b c
let inline table b c = domEl "table" b c
let inline tbody b c = domEl "tbody" b c
let inline td b c = domEl "td" b c
let inline textarea b c = domEl "textarea" b c
let inline tfoot b c = domEl "tfoot" b c
let inline th b c = domEl "th" b c
let inline thead b c = domEl "thead" b c
let inline time b c = domEl "time" b c
let inline title b c = domEl "title" b c
let inline tr b c = domEl "tr" b c
let inline u b c = domEl "u" b c
let inline ul b c = domEl "ul" b c
let inline var b c = domEl "var" b c
let inline video b c = domEl "video" b c

// Void element
let inline area b = voidEl "area" b
let inline ``base`` b = voidEl "base" b
let inline br b = voidEl "br" b
let inline col b = voidEl "col" b
let inline embed b = voidEl "embed" b
let inline hr b = voidEl "hr" b
let inline img b = voidEl "img" b
let inline input b = voidEl "input" b
let inline keygen b = voidEl "keygen" b
let inline link b = voidEl "link" b
let inline menuitem b = voidEl "menuitem" b
let inline meta b = voidEl "meta" b
let inline param b = voidEl "param" b
let inline source b = voidEl "source" b
let inline track b = voidEl "track" b
let inline wbr b = voidEl "wbr" b

// SVG elements
let inline svg b c = svgEl "svg" b c
let inline circle b c = svgEl "circle" b c
let inline clipPath b c = svgEl "clipPath" b c
let inline defs b c = svgEl "defs" b c
let inline ellipse b c = svgEl "ellipse" b c
let inline g b c = svgEl "g" b c
let inline image b c = svgEl "image" b c
let inline line b c = svgEl "line" b c
let inline linearGradient b c = svgEl "linearGradient" b c
let inline mask b c = svgEl "mask" b c
let inline path b c = svgEl "path" b c
let inline pattern b c = svgEl "pattern" b c
let inline polygon b c = svgEl "polygon" b c
let inline polyline b c = svgEl "polyline" b c
let inline radialGradient b c = svgEl "radialGradient" b c
let inline rect b c = svgEl "rect" b c
let inline stop b c = svgEl "stop" b c
let inline text b c = svgEl "text" b c
let inline tspan b c = svgEl "tspan" b c

/// Cast a string to a Inferno element (erased in runtime)
let [<Emit("$0")>] str (s: string): InfernoElement = unbox s
/// Cast an option value to a Inferno element (erased in runtime)
let [<Emit("$0")>] opt (o: InfernoElement option): InfernoElement = unbox o

// Class list helpers
let classBaseList std classes =
    classes
    |> List.fold (fun complete -> function | (name,true) -> complete + " " + name | _ -> complete) std
    |> ClassName

let classList classes = classBaseList "" classes
