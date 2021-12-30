ClearAll[shortInputForm];
shortInputForm[expr_]:=With[{
 $GraphicalPrimitives=AffineHalfSpace|AffineSpace|AffineTransform|Annulus|Arrow|ArrowBox|AttachCell|AxisObject|Ball|BarLegend|BezierCurve|BezierCurveBox|
   BooleanRegion|BoundaryMeshRegion|BSplineCurve|BSplineCurveBox|BSplineSurface|BSplineSurface3DBox|Button|ButtonBar|ButtonBox|CapsuleShape|Circle|CircleBox|
   Cone|ConeBox|ConicHullRegion|ConicHullRegion3DBox|ConicHullRegionBox|CSGRegion|Cube|Cuboid|CuboidBox|Cylinder|CylinderBox|DirectedEdge|Disk|DiskBox|DiskSegment|
   Dodecahedron|Ellipsoid|EmptyRegion|ErrorBox|FilledCurve|FilledCurveBox|FilledTorus|FrameBox|Framed|GeoCircle|GeoDisk|GeoGraphics|GeoMarker|
   GeometricTransformation|GeometricTransformation3DBox|GeometricTransformationBox|GeoPath|GeoVector|GeoVisibleRegionBoundary|Graph|Graph3D|
   Graphics|Graphics3D|Graphics3DBox|GraphicsBox|GraphicsComplex|GraphicsComplex3DBox|GraphicsComplexBox|GraphicsGroup|GraphicsGroupBox|
   HalfLine|HalfPlane|HalfSpace|Hexahedron|HexahedronBox|HighlightGraph|Hyperlink|Hyperplane|Icosahedron|ImplicitRegion|InfiniteLine|InfinitePlane|
   Inset|Inset3DBox|InsetBox|JoinedCurve|JoinedCurveBox|Labeled|Legended|Line|LineBox|LineLegend|MeshRegion|Octahedron|Pane|Panel|PanelBox|
   Parallelepiped|Parallelogram|ParametricRegion|Placed|Point|PointBox|PointLegend|Polygon|Polygon3DBox|PolygonBox|Polyhedron|Prism|PrismBox|Pyramid|PyramidBox|
   Raster|Raster3D|Raster3DBox|RasterBox|RawBoxes|Rectangle|RectangleBox|Region|RegularPolygon|Rotate|RotationTransform|Scale|Simplex|Sound|SoundNote|
   Sphere|SphereBox|SphericalShell|StadiumShape|SurfaceGraphics|SwatchLegend|Tetrahedron|TetrahedronBox|Text|Text3DBox|TextBox|Texture|Translate|Triangle|Tube|TubeBox,
 $DynamicalElements=ActionMenu|ActionMenuBox|AnimatedImage|Animator|AnimatorBox|Annotation|Audio|Button|ButtonBox|Checkbox|CheckboxBar|ClickPane|Clock|ClockGauge|
   ColorSetter|ColorSlider|Control|ControlActive|CurrentScreenImage|CurrentValue|Deploy|Dynamic|DynamicBox|DynamicGeoGraphics|DynamicLocation|
   DynamicModule|DynamicModuleBox|DynamicName|DynamicNamespace|DynamicWrapper|DynamicWrapperBox|EventHandler|EventHandlerTag|FileNameSetter|FlipView|
   FormBox|FormObject|GestureHandler|GestureHandlerTag|HorizontalGauge|Hyperlink|InputField|IntervalSlider|Locator|LocatorPane|LocatorPaneBox|Manipulate|
   MouseAnnotation|MouseAppearance|Mouseover|MousePosition|Opener|OpenerView|PaneSelector|Placeholder|Play|Point|PointBox|PopupMenu|PopupMenuBox|PopupView|
   ProgressIndicator|RadioButton|RadioButtonBar|Refresh|Setter|SetterBar|SetterBox|Slider|Slider2D|Slider2DBox|SliderBox|SlideView|StatusArea|Toggler|
   Tooltip|TooltipBox|TouchPosition|Trigger|VerticalSlider,
 $ColorHeads=CMYKColor|ConicGradientFilling|Darker|DirectionalLight|Directive|Glow|Specularity|GoochShading|GrayLevel|Hue|LABColor|LCHColor|Lighter|
   LUVColor|Opacity|RGBColor|XYZColor,
 $TypesettingNeeded=List|Rule|RuleDelayed|Times|Plus|Power|Sqrt|Exp,
 $DoNotShorten=PlotRange|PlotRangePadding|ImagePadding|ImageMargins|FrameMargins|Spacings|FrameMargins|CellMargins|CellFrameMargins
},
Style[Replace[expr,{
(* Removing empty sublists: http://stackoverflow.com/a/6563973/590388 *)
x_List:>DeleteCases[x,{}],
(* Protecting graphical primitives and they BoxForms from the typesetting and highlighting them *)
s:($GraphicalPrimitives):>Style[s,Bold,StripOnInput->True],
(* Protecting and highlighting dynamical constructs *)
s:($DynamicalElements):>Style[s,Darker@Red,Bold,StripOnInput->True],
(* Protecting graphical directives from typesetting *)
s:Except[$TypesettingNeeded|$ColorHeads|$DoNotShorten,_Symbol]:>Style[s,StripOnInput->True]},{0,Infinity},Heads->True]/.
  {
  (* Protecting graphics option values from shortening *)
   pr:_[$DoNotShorten,_]:>pr,
  (* Protecting color directives from shortening (can have more than 3 arguments) *)
   col:$ColorHeads[__]:>col,
  (* Shortening numerical arrays *)
   lst:{x_,y__}/;ArrayQ[lst,3,NumberQ]:>
    {x/.{a_,b__}:>
    {a/.v:{c_,d__}/;Length[v]>4:>
       {c,Interpretation[Style[Skeleton[Length[{d}]],Gray,Selectable->False],Sequence@@{d}]},
     Interpretation[Style[Skeleton[Length[{b}]],Gray,Selectable->False],Sequence@@{b}]
    },
     Interpretation[Style[Skeleton[Length[{y}]],Gray,Selectable->False],Sequence@@{y}]
    },
   lst:{x_,y__}/;MatrixQ[lst,NumberQ]&&Length[lst]>2:>
    {x/.v:{a_,b__}/;Length[v]>3:>
       {a,Interpretation[Style[Skeleton[Length[{b}]],Gray,Selectable->False],Sequence@@{b}]},
     Interpretation[Style[Skeleton[Length[{y}]],Gray,Selectable->False],Sequence@@{y}]
    },
   lst:{x_,y__}/;VectorQ[lst,NumberQ]&&Length[lst]>3:>
       {x,Interpretation[Style[Skeleton[Length[{y}]],Gray,Selectable->False],Sequence@@{y}]},
  (* Shortening large lists of colors *)
   colorList:{Repeated[$ColorHeads[__],{11,Infinity}]}:>
    {First[colorList],Interpretation[Style[Skeleton[Length[colorList]-1],Gray,Selectable->False],Sequence@@Rest[colorList]]}},
    PrintPrecision->3,StripOnInput->True,ShowStringCharacters->True,ShowAutoStyles->True]];
