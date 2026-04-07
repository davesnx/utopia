  $ utopia.markdown <<\EOF | rg -o '<table>|<thead>|<tbody>|<th|<td|utopia-markdown-align-left|utopia-markdown-align-center|utopia-markdown-align-right'
  > | Name | Score | Note |
  > |:-----|:-----:|-----:|
  > | Ada | 10 | Great |
  > EOF
  <table>
  <thead>
  <th
  utopia-markdown-align-left
  <th
  utopia-markdown-align-center
  <th
  utopia-markdown-align-right
  <tbody>
  <td
  utopia-markdown-align-left
  <td
  utopia-markdown-align-center
  <td
  utopia-markdown-align-right
