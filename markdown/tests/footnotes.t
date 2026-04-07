  $ utopia.markdown <<\EOF | rg -o 'doc-noteref|doc-backlink|class="footnotes"|id="fn-1"|id="ref-1-fn-1"|id="ref-2-fn-1"'
  > Footnote once[^1] and again[^1].
  > 
  > [^1]: Footnote text.
  > EOF
  id="ref-1-fn-1"
  doc-noteref
  id="ref-2-fn-1"
  doc-noteref
  class="footnotes"
  id="fn-1"
  doc-backlink
  doc-backlink
