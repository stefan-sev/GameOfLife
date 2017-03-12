port module Stylesheets exposing (..)

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import Styles


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    Css.File.toFileStructure [ ( "main.css", Css.File.compile [ Styles.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files cssFiles
