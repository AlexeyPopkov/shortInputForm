ClearAll[shortInputForm];
shortInputForm[expr_]:=With[{
 $GraphicalPrimitives=Point|PointBox|Line|LineBox|Arrow|ArrowBox|Rectangle|RectangleBox|
   Parallelogram|Triangle|JoinedCurve|JoinedCurveBox|FilledCurve|FilledCurveBox|
   StadiumShape|DiskSegment|Annulus|BezierCurve|BezierCurveBox|BSplineCurve|BSplineCurveBox|
   BSplineSurface|BSplineSurface3DBox|SphericalShell|CapsuleShape|Raster|RasterBox|Raster3D|Raster3DBox|
   Polygon|PolygonBox|RegularPolygon|Disk|DiskBox|Circle|CircleBox|Sphere|SphereBox|Ball|
   Ellipsoid|Cylinder|CylinderBox|Tetrahedron|TetrahedronBox|Cuboid|CuboidBox|Parallelepiped|
   Hexahedron|HexahedronBox|Prism|PrismBox|Pyramid|PyramidBox|Simplex|ConicHullRegion|ConicHullRegionBox|
   Hyperplane|HalfSpace|AffineHalfSpace|AffineSpace|ConicHullRegion3DBox|Cone|ConeBox|InfiniteLine|
   InfinitePlane|HalfLine|InfinitePlane|HalfPlane|Tube|TubeBox|GraphicsComplex|GraphicsComplexBox|
   GraphicsGroup|GraphicsGroupBox|GeoGraphics|Graphics|GraphicsBox|Graphics3D|Graphics3DBox|
   MeshRegion|BoundaryMeshRegion|GeometricTransformation|GeometricTransformationBox|Rotate|Translate|Scale|
   SurfaceGraphics|Text|TextBox|Inset|InsetBox|Inset3DBox|Panel|PanelBox|Legended|Placed|LineLegend|Texture,
 $DynamicalElements=Dynamic|DynamicBox|Refresh|ActionMenu|ActionMenuBox|Hyperlink|FileNameSetter|Button|ButtonBox|
   Clock|Tooltip|TooltipBox|StatusArea|CurrentValue|Control|Slider|SliderBox|Slider2D|Slider2DBox|IntervalSlider|HorizontalGauge|
   LocatorPane|LocatorPaneBox|ClickPane|Animator|AnimatorBox|Trigger|Manipulate|DynamicModule|DynamicModuleBox|
   Mouseover|Annotation|MouseAnnotation|PopupView|PopupMenu|PopupMenuBox|SetterBar|RadioButtonBar|RadioButton|
   Setter|SetterBox|Checkbox|PaneSelector|Toggler|Opener|OpenerView|SlideView|FlipView|
   EventHandler|EventHandlerTag|GestureHandler|GestureHandlerTag|TouchPosition|ControlActive,
 $ColorHeads=RGBColor|Hue|GrayLevel|CMYKColor|XYZColor|LABColor|LCHColor|LUVColor,
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
       {x,Interpretation[Style[Skeleton[Length[{y}]],Gray,Selectable->False],Sequence@@{y}]}
  }/.{(* Shortening large lists of colors *)
   colorList:{Repeated[$ColorHeads[__],{11,Infinity}]}:>
    {First[colorList],Interpretation[Style[Skeleton[Length[colorList]-1],Gray,Selectable->False],Sequence@@Rest[colorList]]}},
    PrintPrecision->3,StripOnInput->True,ShowStringCharacters->True,ShowAutoStyles->True]];
