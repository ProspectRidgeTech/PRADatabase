module PRA.Page.Theme where
import PRA.Utils
import PRA.App
import Yesod

praTheme :: WidgetT PRA IO ()
praTheme = do
        setTitle "PRA Student Database"
        toWidgetHead
            [hamlet|<link rel="icon" type="image/x-icon" href="http://www.prospectridgeacademy.org/favicon.ico"/>|]
        toWidgetHead
            [lucius|
                body {
                    background-image: url(@{R bgPatternPRA_png});
                }
                hr {
                    width: 40%;
                    border-style: dashed;
                    border-width: 2px;
                }
                a {
                    text-decoration: none;
                    color: darkblue;
                }
                a.hidden {
                    color: black;
                }
                a.hidden:hover {
                    color: darkblue;
                }
                .results, .formbox {
                    margin: auto;
                    width: 60%;
                    text-align: center;
                    font-family: sans-serif;
                    background-color: rgba(85, 85, 85, 0.4);
                    border: 10px groove gold;
                    padding: 10px;
                    p, label {
                        font-size: 95%;
                    }
                }
                .formbox {
                    line-height: 200%;
                    label, input, select, textarea {
                        margin: auto;
                        width: 20%;
                        display: inline-block;
                    }
                    .alert {
                        color: darkred;
                        line-height: 50%;
                    }
                    textarea {
                        resize: none;
                        height: 5em;
                    }
                    .required *,.optional * {
                        vertical-align: middle;
                    }
                }
            |]
