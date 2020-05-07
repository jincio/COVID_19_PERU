(* ::Package:: *)

(* ::Section:: *)
(*Nice list plot*)


Options[referenceNiceListLogPlot] = {
	"Size" -> 800,
	"StartValue" -> 10,
	"Lines" -> {1, 2, 3, 4, 5, 7, 14},
	"PlotStyleRules" -> None,
	"Copyright" -> Nothing,
	"Title" -> None,
	"FrameLeft" -> Automatic,
	"FrameBottom" -> Automatic,
	"FrameTop" -> Automatic,
	"ExtraOptions" -> {},
	"Language" -> "es"
};


referenceNiceListLogPlot[data_, options:OptionsPattern[]] := Module[{plot1, plotRangebase, plotRangebase2, propRanges, nFinal, 
  angles, xTextPos, texts, aspect, b, epilog, 
  plotRangesDiff, size, lines, plotStyle
  },
 b = OptionValue["StartValue"];
 size = OptionValue["Size"];
 lines = OptionValue["Lines"];
 plotStyle = OptionValue["PlotStyleRules"];
 plot1 = With[{datafinal = prepareDataPoints[data, b]}, 
     ListLogPlot[datafinal, OptionValue["ExtraOptions"], PlotLabels -> None, 
      PlotLegends -> Automatic, ImageSize -> size, Joined -> True, 
      PlotStyle -> Replace[plotStyle, {None -> Automatic, rules:({(_Rule|_RuleDelayed)..}|_Association) :> Replace[Keys[datafinal], rules, {1}]}], 
      FrameLabel -> {{Replace[OptionValue["FrameLeft"], Automatic -> "casos totales"], 
         None}, {Column[{Replace[OptionValue["FrameBottom"], {Automatic :> "d\[IAcute]as a partir del caso " <> 
            IntegerString[b], t_TemplateObject :> t[<|"base"->b|>]}], OptionValue["Copyright"]}, 
          Alignment -> Center], 
          Replace[OptionValue["FrameTop"], Automatic :>
         Column[{Style["tendencia del total de casos COVID-19", 
            Large], Style["nacional y por departamento", Larger]}, 
          Alignment -> Center]]}}, 
      Frame -> 
       True]] /. (LegendLayout -> _) :> (LegendLayout -> (Grid[#, 
         Alignment -> Left] &));
 plotRangebase = PlotRange /. AbsoluteOptions[plot1];
 plotRangebase2 = MapAt[Exp, plotRangebase, {2}];
 plotRangesDiff = Subtract @@@ plotRangebase;
 propRanges = Divide @@ (plotRangesDiff);
 nFinal = plotRangebase2[[1, 2]] // Round;
 aspect = AspectRatio /. AbsoluteOptions[plot1];
 angles = Table[ArcTan[1/propRanges, aspect Log[2^(1/line)]], {line, lines}];
 xTextPos = Table[Min[1 + line Log[2, plotRangebase2[[2, 2]]/b], nFinal], {line, lines}];
 texts = MapThread[
	Text[
		Rotate[Style[lineToText[OptionValue["Language"], #1], FontSize -> Scaled[10/size]], #2],
		{#3, Log[b 2^((#3 - 1)/#1)]},
		{1, 1} + If[#2 < .2, .1, .15] {1, -1/Tan[#2]}
	]&, {lines, angles, xTextPos}];
 epilog = {{Opacity[.8], Directive[Black, Dashed], Table[HalfLine[{{1, Log[b]}, {2, Log[b] + Log[2]/d}}], {d, lines}]}, Opacity[.5], texts};
 Show[plot1, Epilog -> epilog] /. HoldPattern[Legended[g_,Placed[lg_,___],opts___]]:>Legended[g,Placed[lg,{{0.045,.98},{0,1}}],opts]
];


lineToText["es", 1] := "duplica cada d\[IAcute]a";
lineToText["es", 7] := "duplica cada semana";
lineToText["es", i_] := If[Mod[i, 7] === 0,
	"duplica cada "<>IntegerName[i/7, {"Spanish", "Cardinal"}]<>" semanas",
	"duplica cada "<>IntegerName[i, {"Spanish", "Cardinal"}]<>" d\[IAcute]as"
];


lineToText["en", 1] := "doubles every day";
lineToText["en", 7] := "doubles every week";
lineToText["en", i_] := If[Mod[i, 7] === 0,
	"doubles every "<>IntegerName[i/7, {"English", "Cardinal"}]<>" weeks",
	"doubles every "<>IntegerName[i, {"English", "Cardinal"}]<>" days"
];


prepareDataPoints[timeseriesAssoc_, b_] := SortBy[DeleteCases[
         With[{v = Values[#]}, 
            (*removeMissingAddRange@*)Drop[v, LengthWhile[v, LessThan[b]]]] & /@ 
          timeseriesAssoc, {} | {_}], -Last[#] &]


removeMissingAddRange[values_] := DeleteCases[Thread[{Range[values], values}/.Plus[___,_Missing|Times[_,_Missing]]:>Missing[]], {_, _?MissingQ}]


(* ::Section:: *)
(*Funciones para Mapa*)


spanishDate[date_]:=Block[{System`DateStringDump`$DateStringLocale="es"},DateString[date,{"DayShort"," de ","MonthName"," de ","Year"}]]


spanishDate[date_, format_]:=Block[{System`DateStringDump`$DateStringLocale="es"},DateString[date,format]]


rasterConform[plots_] := With[
	{rast=Rasterize/@plots},
	{mincrop=Floor[(ImageDimensions/@rast)[[All,2]]//Max,2],xdim=Floor[(ImageDimensions/@rast)[[All,1]]//Max,2]},
	ImageCrop[ImageCrop@#,{xdim,mincrop},Top,Padding->White]&/@rast
];
