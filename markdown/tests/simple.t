Heading

  $ utopia.markdown <<\EOF
  > # This is Markdown
  > ## This is Markdown
  > ### This is Markdown
  > #### This is Markdown
  > ##### This is Markdown
  > ###### This is Markdown
  > 
  > EOF
  <h1>This is Markdown</h1><h2>This is Markdown</h2><h3>This is Markdown</h3><h4>This is Markdown</h4><h5>This is Markdown</h5><h6>This is Markdown</h6>

  $ utopia.markdown <<\EOF
  > [Markdown](http://daringfireball.net/projects/markdown/)
  > [Markdown](http://daringfireball.net/projects/markdown/) lets you write content in a really natural way.
  > 
  > EOF
  <p><a aria-hidden="false" href="http://daringfireball.net/projects/markdown/">Markdown</a> <a aria-hidden="false" href="http://daringfireball.net/projects/markdown/">Markdown</a> lets you write content in a really natural way.</p>

  $ utopia.markdown <<\EOF
  > Soft
  > wrapped text
  > 
  > EOF
  <p>Soft wrapped text</p>

  $ utopia.markdown <<\EOF
  > # UL
  > * You can have lists, like this one
  > * Make things **bold** or *italic*
  > * Embed snippets of `code`
  > * Create [links](/)
  > * ...
  > 
  > EOF
  <h1>UL</h1><ul><li><p>You can have lists, like this one</p></li><li><p>Make things <strong>bold</strong> or <em>italic</em></p></li><li><p>Embed snippets of <code class="utopia-inline-code">code</code></p></li><li><p>Create <a aria-hidden="false" href="/">links</a></p></li><li><p>...</p></li></ul>

  $ utopia.markdown <<\EOF
  > # OL
  > 1. You can have lists, like this one
  > 2. Make things **bold** or *italic*
  > 3. Embed snippets of `code`
  > 4. Create [links](/)
  > 5. ...
  > 
  > EOF
  <h1>OL</h1><ol><li><p>You can have lists, like this one</p></li><li><p>Make things <strong>bold</strong> or <em>italic</em></p></li><li><p>Embed snippets of <code class="utopia-inline-code">code</code></p></li><li><p>Create <a aria-hidden="false" href="/">links</a></p></li><li><p>...</p></li></ol>

  $ utopia.markdown <<\EOF
  > > In a few moments he was barefoot, his stockings folded in his pockets and his
  <blockquote><p>In a few moments he was barefoot, his stockings folded in his pockets and his</p></blockquote>

  $ utopia.markdown <<\EOF | rg -o 'class="ochre utopia-markdown-code-block"|class="utopia-markdown-code"|console|globalThis'
  > ```javascript
  > console.log(globalThis);
  > ```
  > 
  > EOF
  class="ochre utopia-markdown-code-block"
  class="utopia-markdown-code"
  console
  globalThis

  $ utopia.markdown <<\EOF | rg -o 'class="line"' | wc -l
  > ```javascript
  > console.log("a");
  > console.log("b");
  > ```
  > 
  > EOF
  2

  $ utopia.markdown <<\EOF
  > ```
  > Generic code block
  > ```
  > 
  > EOF
  <pre class="utopia-markdown-code-block"><code class="utopia-markdown-code">Generic code block
  </code></pre>

  $ utopia.markdown <<\EOF
  > <script type="text/javascript">
  > // JavaScript example
  > document.getElementById("demo").innerHTML = "Hello JavaScript!";
  > </script>
  > 
  > EOF
  <div><script type="text/javascript">
  // JavaScript example
  document.getElementById("demo").innerHTML = "Hello JavaScript!";
  </script></div>

  $ utopia.markdown <<\EOF
  > Hola
  > 
  > -------------
  > 
  > OCaml
  > 
  > EOF
  <p>Hola</p><hr /><p>OCaml</p>

  $ utopia.markdown <<\EOF
  > <small>Sample content borrowed from [markdown-to-jsx](https://markdown-to-jsx.quantizor.dev/), thanks! ❤️</small>
  <p>&lt;small&gt;Sample content borrowed from <a aria-hidden="false" href="https://markdown-to-jsx.quantizor.dev/">markdown-to-jsx</a>, thanks! ❤️&lt;/small&gt;</p>
