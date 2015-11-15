// PEG.js grammar for WebIDL
// PES.js is a simple parser generator based on Parsing Expression Grammar

start
  = ws defs:definitions* ws
    { return defs; }
        
definitions
  = def:(Package / EnumDef / Comment / ClassDef / expression)
    { return def; }
    
Package 'package'
  = ws 'package' ws packageName:identifier EOS {
      return {
        type: 'package'
      , name: packageName
      }
    }
    
EnumDef
  = "enum" ws enumName:identifier ws "{"
      ws
    "}" ws ";"
  { return "var " + enumName.join("") + "= function()"; }
  
ClassDef
  = "class" ws className:identifier ws "{"
      ws
    "}" ws ";"
  { return "var " + className.join("") + "= function()"; }
  
expression = 
  cat:category* prop:property* {return JSON.stringify({"category": cat,
  property:prop, undefined}); }
  
category 
  = ws '@' symbol:word ws { return symbol; }

property 
  = ws '#' symbol:word ws phrase:alphanumeric ws { return [symbol, phrase]; }
  
ws "whitespace" = [' '\t\n\r]*

word = wholeWord:[a-zA-Z]* { return wholeWord.join(""); }

digit = wholeDigit:[0-9]* { return wholeDigit.join(""); }

alphanumeric = wholePhrase:[a-zA-Z0-9]* { return wholePhrase.join(""); }

identifier = identifierName:[A-Z_a-z]*

Comment 'comment'
  = SingleLineComment / MultiLineComment
  
SingleLineComment
  = '//' (!LineTerminator SourceCharacter)*

SourceCharacter
  = .

LineTerminator
  = [\n\r\u2028\u2029]
  
LineTerminatorSequence 'end of line'
  = '\n'
  / '\r\n'
  / '\r'
  / '\u2028' // line separator
  / '\u2029' // paragraph separator
  
MultiLineComment
  = '/*' !'*' comment:(!'*/' SourceCharacter)* '*/'

MultiLineCommentNoLineTerminator
  = '/*' (!('*/' / LineTerminator) SourceCharacter)* '*/'
  
EOS
  = ws ';'
  / ws LineTerminatorSequence
  / ws &'}'
  / ws EOF

EOF
  = !.
