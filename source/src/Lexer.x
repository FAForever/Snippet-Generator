{
module Lexer where

import Model
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters
$special = [\.\;\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$operators = [=]

tokens :- 
  $white+                             ;
  "function"                          { \_ -> TokenFunction }
  [$alpha$digit]+                     { \s -> TokenIdentifier s }
  "("                                 { \_ -> TokenOpen }
  ")"                                 { \_ -> TokenClose }
  ","                                 { \_ -> TokenComma }
  ":"                                 { \_ -> TokenColon }
  "--".*                              { \s -> TokenComment s}
  "#".*                               { \s -> TokenComment s}
  .                                   ;
{
 
}