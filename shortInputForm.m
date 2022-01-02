ClearAll[shortInputForm];

(* This subroutine "un-evaluates" the supplied expression: 
it converts Atomic compound-type objects (like Graph etc.) into the corresponding unevaluated expressions. 
But it keeps untouched NumericArray and ByteArray. *)
shortInputForm[expr_,Unevaluated]:=Module[{},
    (* Based on Nucleus function by Carl Woll: *)
    (* https://mathematica.stackexchange.com/a/157198/280 *)
    If[!MemberQ[Links[], $AtomLinkForshortInputForm] || LinkReadyQ[$AtomLinkForshortInputForm],
        Quiet @ LinkClose[$AtomLinkForshortInputForm];
        $AtomLinkForshortInputForm = LinkCreate[LinkMode -> Loopback]
    ];
    LinkWrite[$AtomLinkForshortInputForm, expr];
    shortInputForm[LinkRead[$AtomLinkForshortInputForm,HoldComplete],ReleaseHold]
];
(* Handling NumericArray and ByteArray *)
shortInputForm[expr:(_NumericArray|_ByteArray)]:=shortInputForm[ToExpression[ToString[expr,InputForm],InputForm,HoldComplete],ReleaseHold]
(* All other Atoms *)
shortInputForm[expr_?AtomQ]:=shortInputForm[expr,Unevaluated];
(* This subroutine inactivates and formats all Heads in expr, and then shortens long lists *)
shortInputForm[expr_,ReleaseHold]:=With[{
 $Objects=AffineHalfSpace|AffineSpace|AffineTransform|Annulus|Arrow|ArrowBox|AttachCell|AxisObject|Ball|BarLegend|BezierCurve|BezierCurveBox|
   BooleanRegion|BoundaryMeshRegion|BSplineCurve|BSplineCurveBox|BSplineSurface|BSplineSurface3DBox|Button|ButtonBar|ButtonBox|ByteArray|CapsuleShape|Circle|CircleBox|
   Cone|ConeBox|ConicHullRegion|ConicHullRegion3DBox|ConicHullRegionBox|CSGRegion|Cube|Cuboid|CuboidBox|Cylinder|CylinderBox|DirectedEdge|Disk|DiskBox|DiskSegment|
   Dodecahedron|Ellipsoid|EmptyRegion|ErrorBox|FilledCurve|FilledCurveBox|FilledTorus|FrameBox|Framed|GeoCircle|GeoDisk|GeoGraphics|GeoMarker|
   GeometricTransformation|GeometricTransformation3DBox|GeometricTransformationBox|GeoPath|GeoVector|GeoVisibleRegionBoundary|GoochShading|Graph|Graph3D|
   Graphics|Graphics3D|Graphics3DBox|GraphicsBox|GraphicsComplex|GraphicsComplex3DBox|GraphicsComplexBox|GraphicsGroup|GraphicsGroupBox|
   HalfLine|HalfPlane|HalfSpace|Hexahedron|HexahedronBox|HighlightGraph|Hyperlink|Hyperplane|Icosahedron|Image|ImplicitRegion|InfiniteLine|InfinitePlane|
   Inset|Inset3DBox|InsetBox|JoinedCurve|JoinedCurveBox|Labeled|Legended|Line|LineBox|LineLegend|MeshRegion|NumericArray|Octahedron|Pane|Panel|PanelBox|
   Parallelepiped|Parallelogram|ParametricRegion|Placed|Point|PointBox|PointLegend|Polygon|Polygon3DBox|PolygonBox|Polyhedron|Prism|PrismBox|Pyramid|PyramidBox|
   Raster|Raster3D|Raster3DBox|RasterBox|RawBoxes|Rectangle|RectangleBox|Region|RegularPolygon|Rotate|RotationTransform|Scale|Simplex|Sound|SoundNote|SparseArray|
   Sphere|SphereBox|SphericalShell|StadiumShape|SurfaceGraphics|SwatchLegend|Tetrahedron|TetrahedronBox|Text|Text3DBox|TextBox|Translate|Triangle|Tube|TubeBox,
 $DynamicalElements=ActionMenu|ActionMenuBox|AnimatedImage|Animator|AnimatorBox|Annotation|Audio|Button|ButtonBox|Checkbox|CheckboxBar|ClickPane|Clock|ClockGauge|
   ColorSetter|ColorSlider|Control|ControlActive|CurrentScreenImage|CurrentValue|Deploy|Dynamic|DynamicBox|DynamicGeoGraphics|DynamicLocation|
   DynamicModule|DynamicModuleBox|DynamicName|DynamicNamespace|DynamicWrapper|DynamicWrapperBox|EventHandler|EventHandlerTag|FileNameSetter|FlipView|
   FormBox|FormObject|GestureHandler|GestureHandlerTag|HorizontalGauge|Hyperlink|InputField|IntervalSlider|Locator|LocatorPane|LocatorPaneBox|Manipulate|
   MouseAnnotation|MouseAppearance|Mouseover|MousePosition|Opener|OpenerView|PaneSelector|Placeholder|Play|Point|PointBox|PopupMenu|PopupMenuBox|PopupView|
   ProgressIndicator|RadioButton|RadioButtonBar|Refresh|Setter|SetterBar|SetterBox|Slider|Slider2D|Slider2DBox|SliderBox|SlideView|StatusArea|Toggler|
   Tooltip|TooltipBox|TouchPosition|Trigger|VerticalSlider,
 $ColorHeads=CMYKColor|Darker|Directive|GrayLevel|Hue|LABColor|LCHColor|Lighter|
   LUVColor|Opacity|RGBColor|XYZColor,
 $TypesettingNeeded=List|Association|Rule|RuleDelayed|Times|Plus|Power|Sqrt|Exp,
 $DoNotShorten=PlotRange|PlotRangePadding|ImagePadding|ImageMargins|Spacings|FrameMargins|CellMargins|CellFrameMargins,
 $SpecialDirectives=VertexColors|VertexNormals|VertexTextureCoordinates|
   Texture|BackFaceTexture|FrontFaceTexture|ConicGradientFilling|LinearGradientFilling|RadialGradientFilling|HatchFilling|PatternFilling|
   Lighting|DirectionalLight|AmbientLight|PointLight|SpotLight|
   ContourShading|GoochShading|HalftoneShading|HatchShading|MaterialShading|MeshShading|Shading|StippleShading|ToonShading|
   Specularity|Glow|
   ViewAngle|ViewCenter|ViewMatrix|ViewPoint|ViewPointSelectorSettings|ViewPort|ViewProjection|ViewRange|ViewVector|ViewVertical
},
Style[ReleaseHold @ Replace[expr,
  (* Rules for inactivating and styling heads and directives; also removing empty sublists *)
  {
  (* Removing empty sublists: http://stackoverflow.com/a/6563973/590388 *)
  x_List:>DeleteCases[x,{}],
  (* Protecting graphical heads, primitives and they BoxForms from the typesetting and highlighting them *)
  s:($Objects):>Style[s,Bold,StripOnInput->True],
  (* Protecting and highlighting dynamical constructs *)
  s:($DynamicalElements):>Style[s,Darker@Red,Bold,StripOnInput->True],
  (* Highlighting selected special directives *)
  s:($SpecialDirectives):>Style[s,Darker@Green,Bold,StripOnInput->True],  
  (* Protecting remaining symbols from typesetting *)
  s:Except[$TypesettingNeeded|$ColorHeads|$DoNotShorten|HoldComplete,_Symbol]:>Style[s,StripOnInput->True]
  },{1,Infinity},Heads->True]/.
    (* Rules for simplifying option lists (all Symbols are already inactivated) *)
    {
    (h:Style[Graphics|GraphicsBox|Graphics3D|Graphics3DBox,___])[first_,rest__]:>h[first,SortBy[DeleteDuplicatesBy[Flatten[{rest}],First[#,#]&],ToString@First[#,#]&]]
    }/.
    (* Rules for code shortening *)
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
    
shortInputForm[expr_]:=shortInputForm[HoldComplete[expr],ReleaseHold];
