(* ::Package:: *)

(* ::Section:: *)
(*NOTE: Please open this file in a pure text editor for better indenting and alignment.*)

(* ::Subtitle:: *)
(*begin package*)


BeginPackage["CompileNotingTools`", { "CompiledFunctionTools`" }]


(* ::Subsection:: *)
(*Usage message*)

Clear[CompileNotingTools`Private`usageMsg]
CompileNotingTools`Private`usageMsg = 
  MapAt[
        Style[ #, Bold ] &,
        {
         "CompileNoting[ CompiledFunction[...] ]",
         " is a fancy formatter which generates a sidenoted ",
         "Input Cell",
         " of the inputted ",
         "CompiledFunction[...]",
         " ."
        },
        {1, 3, 5} // {#}\[Transpose] &
       ] // Row

Clear[CompileNotingTools`Private`exampleMsg]
CompileNotingTools`Private`exampleMsg = 
  RowBox[{RowBox[{"Compile", "[", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"n", ",", "_Integer"}], "}"}],",", "x"}], "}"}], ",", FractionBox[RowBox[{"n", "+", "x"}], RowBox[{"{", RowBox[{"3", ",", "7"}], "}"}]]}], "]"}], "//","CompileNoting"}] //
     RawBoxes // Style[#, "Input", FontWeight -> Plain ] & //
       Column[{
               Style["Usage Example: ", Bold, 14],
               #
              }] &

CompileNoting::usage = 
 {
  CompileNotingTools`Private`usageMsg,
  CompileNotingTools`Private`exampleMsg
 } // Column[ #, Dividers -> Center, FrameStyle -> GrayLevel[.7], Spacings -> 3 ]& //
        ToString[ #, StandardForm ]&

(* ::Subtitle:: *)
(*code*)

Begin["`Private`"]

(* ::Subsection:: *)
(*Main function*)

Clear[CompileNoting]
CompileNoting[compfunc_CompiledFunction] :=
  Module[{constBlock,regStatBlock, codeBlock, TEMPHEAD},
          
        (*       vnfBlock = versionAndFlagBlockFormat[ compfunc[[1]]           ]; *)
        (* inputTypeBlock =      inputTypeBlockFormat[ compfunc[[2]]           ]; *)
          ioRegisterBlock =     ioRegisterBlockFormat[ compfunc[[3]]           ];
               constBlock =          constBlockFormat[ compfunc[[4]]           ];
             regStatBlock =            regBlockFormat[ compfunc[[5]]           ];
                codeBlock =           codeBlockFormat[ compfunc[[6]], compfunc ];
        (*  origFuncBlock =       origFuncBlockFormat[ compfunc[[7]]           ]; *)
            origFuncBlock = Framed[Style["Uncompiled Code omitted", 12, GrayLevel[.4], "Text"], FrameStyle -> Hue[0.11, 0.5, 0.85]] // Inactive[Function][Null, #] & // Activate // ToBoxes;
             falbackBlock = (Failure["Execution Failed", "Message" -> "The compiled code cannot be executed properly."] &) // ToBoxes;
          
         ToBoxes[
                 (* Function structure start: *)
                 ReplacePart[
                             TEMPHEAD @@ compfunc,
                             {(*  1 ->           "VNF_BLOCK"  ,  *)
                              (*  2 ->    "INPUT_TYPE_BLOCK"  ,  *)
                                  3 ->   "IO_REGISTER_BLOCK"  ,
                                  4 ->         "CONST_BLOCK"  ,
                                  5 ->      "REG_STAT_BLOCK"  ,
                                  6 ->          "CODE_BLOCK"  ,
                                  7 -> "ORIG_FUNCTION_BLOCK"  ,
                                  8 ->      "FALLBACK_BLOCK"  ,
                                  (* the 9th part is for C-compiled function, will be the LibraryFunction[...]: *)
                                  9 -> If[ Length[compfunc] == 9, InputForm[ compfunc[[9]] ] ]
                             }
                            ]
                 (* Function structure ends.
                    Boxes start: *)
                ] /.
                    {
                      (*           "\"VNF_BLOCK\"" ->        vnfBlock   , *)
                      (*    "\"INPUT_TYPE_BLOCK\"" ->  inputTypeBlock   , *)
                           "\"IO_REGISTER_BLOCK\"" -> Inactive[Sequence][  indentnl, ioRegisterBlock  ]   ,
                                 "\"CONST_BLOCK\"" -> Inactive[Sequence][  indentnl,      constBlock  ]   ,
                              "\"REG_STAT_BLOCK\"" -> Inactive[Sequence][  indentnl,    regStatBlock  ]   ,
                                  "\"CODE_BLOCK\"" -> Inactive[Sequence][  indentnl,       codeBlock  ]   ,
                         "\"ORIG_FUNCTION_BLOCK\"" -> Inactive[Sequence][  indentnl,   origFuncBlock  ]   ,
                              "\"FALLBACK_BLOCK\"" -> Inactive[Sequence][  indentnl,    falbackBlock  ](* , *)
                    } /. 
                        ToString[TEMPHEAD] -> "CompiledFunction" // Activate //
                            CellPrint[styledCell[#]]&
        ]

(* ::Subsection:: *)
(*Block functions*)

Clear[ioRegisterBlockFormat]
ioRegisterBlockFormat[ioRegisterBlock_]:=
  {
     ToBoxes /@ ioRegisterBlock,
                ioRegisterBlock /. regExprToRegStrRule // cmtBox /@ # &
  }\[Transpose] // 
   Riffle[#, Inactive[List][",", indentnl]] & // Activate //
     RowBox[
            {"{",#,"}"} // Flatten //
                    {
                        #[[1]],   (* "{" *)
                      indentnl,          cmtBox@"Input registers:" ,
                      indentnl,          #[[2 ;; -4]]              ,
                      indentnl,          cmtBox@"Output register:" ,
                      indentnl,          #[[-3 ;; -2]]             ,
                      indentnl,
                       #[[-1]]    (* "}" *)
                    } & // Flatten
   ] &


Clear[constBlockFormat]
constBlockFormat[constBlock_]:=
  If[constBlock==={},
     RowBox[{
                           "{",
             indentnl,        cmtBox["no constant"],
             indentnl,     "}"
            }],
     {
      (* 1: *)
      {
       ToString /@ constBlock[[All,1]],
       constBlock[[All,2]] /. regExprToRegStrRule
      }//MapThread[
                   ReplacePart[  #,  {{1,2},{1,-2}}:>Sequence[]  ]&@
                     cmtBox@
                       GridBox[
                               {{#1,"\[DoubleDownArrow]",#2}}\[Transpose], (* Formatting options: *) GridBoxAlignment->{"Columns"->{{Center}}},DefaultBaseStyle->"Column",GridBoxFrame->{"Columns"->None,"Rows"->None},GridBoxItemSize->{"Columns"->{{Automatic}},"Rows"->{{Automatic}}}
                              ]&,
                   #]&,
      (* 2: *)
      ToBoxes /@ constBlock
     }//
        #\[Transpose]&//
                        RowBox[  {
                                               "{",
                                  indentnl,       cmtBox["Constant(s) in register(s):"],
                                  indentnl,       Riffle[#,","],
                                  indentnl,     "}"
                                 }//Flatten  ]&
    ]


Clear[regBlockFormat]
regBlockFormat[regBlock_]:=
  {
   cmtBox  /@ {booleRegFormat["B"],scalerRegFormat["I"],scalerRegFormat["R"],scalerRegFormat["C"],tensorRegFormat["T"]},
   ToBoxes /@ regBlock
  }//
     MapThread[RowBox[{ReplacePart[#1,{1,-2}:>Sequence[]],#2}]&,#]&//
       RowBox[{"{",indentnl,cmtBox["number of registers:"],indentnl,Riffle[#,","],indentnl,"}"}//Flatten]&


(* codeBlock_ should be the 6th part of a CompiledFunction, i.e. the byte-code List part: *)
Clear[codeBlockFormat]
codeBlockFormat[codeBlock_, compfunc_] :=
 Module[{compiledstr, codestr, codestrboxSet, codeboxSet},
         
         
        compiledstr = compfunc // CompilePrint;
         
         
        (* instruction parts in the CompilePrint-ed String: *)
        codestr = StringCases[compiledstr, 
                              "\n\n" ~~ code : (DigitCharacter .. ~~ "\t" ~~ __) ~~ "\n" ~~ EndOfString :> code
                             ][[1]];
         
         
        (* construct the readable instructions as comments, the result is List of styled Boxes arranged as {{lnum1, instr1}, {lnum2, instr2}, ...}: *)
        codestrboxSet = codestr //
                                   (* x is the line number: *)
                                   StringSplit[#, "\n" ~~ x : DigitCharacter .. ~~ "\t" :> x] & //
                                   MapAt[Sequence @@ {StringTake[#, 1], StringDrop[#, 2]} &, #, 1] & //
                                   cmtBox[instrStrFormat[#]] & /@ # & //
                                   Partition[#, 2] &;
         
         
        (* List of Boxes, which will re-construct the executable byte-codes: *)
        codeboxSet = Cases[codeBlock // ToBoxes, 
                           RowBox[{
                                   "{", 
                                   (* I forget what pattern c is used for... *)
                                   RowBox[{a : (PatternSequence[RowBox[__], ","] ..), b_} | c : {_String ..}],
                                   "}"
                                  }] :> 
                                       If[SameQ[{c}, {}], {a, b}[[1 ;; -1 ;; 2]], {RowBox[c]}],
                            {0}
                          ][[1]];
         
         
         
        (* construct the instruction-"noted" byte-code List: *)
        MapThread[{
                   (* the styled instruction: *)
                   ReplacePart[#1[[1]], {1, 3} -> lineNumFormat[#1[[1, 1, 3]]]],
                   ReplacePart[#1[[2]], {1, 3} -> codebodyFormat[#1[[2, 1, 3]]]], 
                   (* new line with auto-indent: *)
                   indentnl,
                   "\t\t\t",
                   (* the executable byte-code: *)
                   #2,
                   ",",
                   (* line between two instruction-code groups: *)
                   indentnl,
                   cmtBox[lineNumFormat["--------------------------"]], 
                   indentnl
                  } &,
                  {codestrboxSet, codeboxSet}
                 ] // 
                     RowBox[{
                             "{",
                             indentnl,
                             cmtBox["bytecode block:"], 
                             indentnl,
                             (* delete extra comma, separating line, etc in the last instr-code group: *)
                             MapAt[#[[1 ;; -5]] &, #, -1], 
                             indentnl, 
                             "}"
                            } // Flatten] &

       ]

(* ::Subsection:: *)
(*Semantic formatters*)

Clear[instrStrFormat]
instrStrFormat[codestr_] :=
 StringReplace[codestr,
               {
                (* inefficient instructions:                                                         *)
                   wn : "MainEvaluate"|"CopyTensor"                                                  :>     warningFormat[ wn                                        ],
                (* flow control instructions:                                                        *)
                       StartOfString~~ if   : "if"   ~~"["                                           :>        ctrlFormat[ ToUpperCase@if                            ] ~~ "[",
                   " "|StartOfString~~ goto : "goto" ~~" "~~ lineno : DigitCharacter ..              :> " " ~~ ctrlFormat[ ToUpperCase@goto                          ] ~~ " " ~~ lineNumFormat[lineno],
                (* tensors:                                                                          *)
                   r : ("T("~~ t:"I"|"R"|"C" ~~ rank:DigitCharacter.. ~~")"~~count:DigitCharacter..) :>   tensorRegFormat[ "T"<>count<>":" ~~ SuperscriptBox[scalerRegFormat[t],rank] ],
                (* boole:                                                                            *)
                   r : ("B"~~DigitCharacter..)                                                       :>    booleRegFormat[ r                                         ],
                (* integer, real, complex:                                                           *)
                   r : ("I"|"R"|"C"~~DigitCharacter..)                                               :>   scalerRegFormat[ r                                         ],
                (* void type, such as Null:                                                          *)
                   r : ("V"~~DigitCharacter..)                                                       :>     warningFormat[ r                                         ]
                                        }
              ] //. StringExpression[e__] :> RowBox[{e}]


      ctrlFormat[ ctrl_ ] :=          StyleBox[ ctrl, FontWeight -> "Bold", FontSize ->    ctrlFontSize, FontColor -> RGBColor[.8, 0, 0] ]
   warningFormat[ code_ ] := FrameBox[StyleBox[ code, FontWeight -> "Bold", FontSize -> warningFontSize, FontColor -> RGBColor[.5,.2,.2] ], Background -> RGBColor[ 1,.8,.8], FrameStyle -> RGBColor[.8,.5,.5], FrameMargins -> 1]
 tensorRegFormat[ r_    ] := FrameBox[StyleBox[    r, FontWeight -> "Bold", FontSize ->     regFontSize, FontColor -> RGBColor[ 0,.3,.5] ], Background -> RGBColor[.8, 1, 1], FrameStyle -> RGBColor[.5,.7,.7], FrameMargins -> 1]
 scalerRegFormat[ r_    ] := FrameBox[StyleBox[    r, FontWeight -> "Bold", FontSize ->     regFontSize, FontColor -> RGBColor[.2,.5,.2] ], Background -> RGBColor[.8, 1,.8], FrameStyle -> RGBColor[.5,.8,.5], FrameMargins -> 1]
  booleRegFormat[ r_    ] := FrameBox[StyleBox[    r, FontWeight -> "Bold", FontSize ->     regFontSize, FontColor -> RGBColor[.5,.2,.5] ], Background -> RGBColor[ 1,.8, 1], FrameStyle -> RGBColor[.8,.5,.8], FrameMargins -> 1]


   lineNumFormat[ lno_  ] := StyleBox[ lno, FontColor -> RGBColor[0,.4,.9], FontWeight -> "Bold", FontSlant -> "Italic"]
cmtBracketFormat[ esc_  ] := StyleBox[ esc, FontColor -> GrayLevel[.9]                                                 ]
  codebodyFormat[ code_ ] := StyleBox[code, FontColor -> GrayLevel[.3],     FontWeight -> "Plain"                      ]

(* ::Subsection:: *)
(*Cell generator*)

styledCell[boxs_] := 
 Cell[BoxData[StyleBox[boxs, FontSize -> codeExprFontSize]],
      "Input",
      Background         -> GrayLevel[1],
      CellFrame          -> True, 
      CellBracketOptions -> {"Color" -> RGBColor[.4, 0, 1], "Thickness" -> 2}
     ]


(* ::Subsection:: *)
(*Other helpers*)


Clear[regExprToRegStrRule]
regExprToRegStrRule =
  {regType_Integer, regRank_Integer, regCount_Integer} :>
          Module[{typeStr},
                 typeStr = regType /. Thread[{1, 2, 3, 4} -> Characters["BIRC"]];
                 If[regRank > 0,
                    "T(" <> typeStr <> ToString[regRank] <> ")" <> ToString[regCount],
                            typeStr                             <> ToString[regCount]
                   ] // instrStrFormat
                ]

cmtBox[cmt_] := RowBox[{cmtBracketFormat["(*"], "\t", cmt, "\t", cmtBracketFormat["*)"]}]


Clear[indentnl]
indentnl = "\[IndentingNewLine]"


Clear[codeExprFontSize, ctrlFontSize, warningFontSize, regFontSize]
codeExprFontSize = 10;
    ctrlFontSize = 14;
 warningFontSize = 14;
     regFontSize = 12;



(* ::Subtitle:: *)
(*end package*)


End[ ]
EndPackage[ ]

?CompileNoting
