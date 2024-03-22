Test markdown

  $ utopia.markdown <<\EOF
  > # This is Markdown
  > 
  > #### You can edit me!
  > 
  > [Markdown](http://daringfireball.net/projects/markdown/) lets you write content in a really natural way.
  > 
  >   * You can have lists, like this one
  >   * Make things **bold** or *italic*
  >   * Embed snippets of `code`
  >   * Create [links](/)
  >   * ...
  > 
  > <small>Sample content borrowed from [markdown-to-jsx](https://markdown-to-jsx.quantizor.dev/), thanks! ❤️</small>
  > 
  > Custom handling of code blocks (or any rule!) is possible with the [`renderRule` option](https://github.com/quantizor/markdown-to-jsx#optionsrenderrule). For example, LaTeX support via [`@matejmazur/react-katex`](https://www.npmjs.com/package/@matejmazur/react-katex):
  > 
  > ```latex
  > \mathbb{N} = \{ a \in \mathbb{Z} : a > 0 \}
  > ```
  > 
  > Or any other typical language, using [`highlight.js`](https://highlightjs.org/):
  > 
  > ```javascript
  > function App() {
  >   return <div>Hello world!</div>;
  > }
  > ```
  > 
  > You can even include custom React components if you declare them in the [`overrides` option](https://github.com/quantizor/markdown-to-jsx/blob/main/README.md#optionsoverrides---rendering-arbitrary-react-components).
  > 
  > <MyComponent>Isn't that cool?</MyComponent>
  > EOF
  module Page = {
        let path = "TODO";
      
        [@react.component]
        let make = () => {
         <> <h1>This is Markdown</h1>
  <h4>You can edit me!</h4>
  <p><a href="http://daringfireball.net/projects/markdown/">Markdown</a> lets you write content in a really natural way.</p>
  <ul>
  <li>You can have lists, like this one</li>
  <li>Make things <strong>bold</strong> or <em>italic</em></li>
  <li>Embed snippets of <code>code</code></li>
  <li>Create <a href="/">links</a></li>
  <li>...</li>
  </ul>
  <p><small>Sample content borrowed from <a href="https://markdown-to-jsx.quantizor.dev/">markdown-to-jsx</a>, thanks! ❤️</small></p>
  <p>Custom handling of code blocks (or any rule!) is possible with the <a href="https://github.com/quantizor/markdown-to-jsx#optionsrenderrule"><code>renderRule</code> option</a>. For example, LaTeX support via <a href="https://www.npmjs.com/package/@matejmazur/react-katex"><code>@matejmazur/react-katex</code></a>:</p>
  <pre><code class="language-latex">\mathbb{N} = \{ a \in \mathbb{Z} : a &gt; 0 \}
  </code></pre>
  <p>Or any other typical language, using <a href="https://highlightjs.org/"><code>highlight.js</code></a>:</p>
  <pre><code class="language-javascript">function App() {
    return &lt;div&gt;Hello world!&lt;/div&gt;;
  }
  </code></pre>
  <p>You can even include custom React components if you declare them in the <a href="https://github.com/quantizor/markdown-to-jsx/blob/main/README.md#optionsoverrides---rendering-arbitrary-react-components"><code>overrides</code> option</a>.</p>
  <p><MyComponent>Isn't that cool?</MyComponent></p>
  ; </>
        };
      };
      
      Utopia.Router.register((module Page));
