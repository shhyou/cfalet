import argparse
import re
from cgi import escape

# Arguments:
#   1. string: title
#   2. [string]: helper texts, e.g. `["text0", "text1", "text2"]`
#   3. (marked) code; mark with
#       `<span ng-mouseover="show(NUM)" ng-mouseleave="mover=0">text</span>`
#   4. store 
html_template = """<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
  <title>%s</title>

  <style type="text/css">
    #main_code {
      font-family: Consolas;
      cursor: default;
      white-space: pre;
    }

    #related_text {
      font-family: Consolas;
      font-size: 80%%;
      background-color: rgb(254,248,189);
      position: absolute;
      width: 100px;
      white-space: pre-line;
      border: 1px solid rgb(0,0,0);
    }

    pre > code {
      white-space: pre-wrap;
    }
  </style>

  <script type="text/javascript" src="./angular.min.js"></script>

  <link rel="stylesheet" href="./styles/github.css">
  <script type="text/javascript" src="./highlight.pack.js"></script>
  <script type="text/javascript">
    /* initialize highlight.js */
    hljs.initHighlightingOnLoad();

    function obj(id) { return document.getElementById(id); }

    function initTools() {
      /* manual highlight main code */
      hljs.highlightBlock(obj("main_code"));

      /* start angular.js */
      var app = angular.module("main", []).controller(
        "body",
        function ($scope) {
          $scope.show = function(textid) {
            $scope.textid = textid;
            $scope.mover = 1;
          };
          $scope.related_texts =
%s;
        }
      );
      angular.bootstrap(document, ["main"]);
    }
  </script>
</head>
<body onload="initTools();" ng-controller="body" ng-mousemove="mousex = $event.offsetX; mousey = $event.offsetY">
  <div id="main_code" class="ocaml">
%s
  </div>
  <pre><code>
%s
  </code></pre>
  <div id="related_text" ng-show="mover" ng-init="mover=0; textid=0" ng-attr-style="left: {{mousex + 18}}px; top: {{mousey + 18}}px">{{related_texts[textid]}}</div>
</body>
</html>

"""

def main(inp, out):
  """
  input file format:
     (tagged) code,  {{tag1|code1}} abcd {{tag2|code2}} ...
     =====
     store
     =====
     text0
     ---
     text1
     ---
     text2
     ---
     ...
     ---
     textn
     ---
  """
  with open(inp, "r") as fin:
    mark_template ="<span ng-mouseover='show(%s)' ng-mouseleave='mover=0'>%s</span>%s"
    raw_code, raw_store, raw_texts = fin.read().split("=====\n")
    helper_texts = "[" + ", ".join(repr(s) for s in raw_texts.split("---\n")[:-1]) + "]"

    store = escape(raw_store)

    # pre_code, *tag_codes = raw_code.split("{{")
    raw_code_tmp = raw_code.split("{{")
    pre_code = raw_code_tmp[0]
    tag_codes = raw_code_tmp[1:]

    marks = [mark_template % tuple(re.split("\||\}\}", s)) for s in tag_codes]
    marked_code = pre_code + "".join(marks)
  with open(out, "w") as fout:
    fout.write(html_template % (inp + " - View", helper_texts, marked_code, store))

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='Generate CFA result views')
  parser.add_argument("input", help = "input file name")
  parser.add_argument("-o", "--output", metavar = "output",
                      help = "set output file name; the default is $input.html")
  args = parser.parse_args()
  if args.output is None:
    args.output = (args.input.split("/")[-1]).split("\\")[-1] + ".html"
  main(args.input, args.output)
