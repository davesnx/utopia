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
  > EOF
  module Page = {
        let path = "TODO";
      
        [@react.component]
        let make = () => {
         <> <h1>{React.string("This is Markdown")}</h1>
  <h4>{React.string("You can edit me!")}</h4>
  <p><a href="http://daringfireball.net/projects/markdown/">{React.string("Markdown")}</a>{React.string(" lets you write content in a really natural way.")}</p>
  <ul>
  <li>{React.string("You can have lists, like this one")}</li>
  <li>{React.string("Make things ")}<strong>{React.string("bold")}</strong>{React.string(" or ")}<em>{React.string("italic")}</em></li>
  <li>{React.string("Embed snippets of ")}<code>{React.string("code")}</code></li>
  <li>{React.string("Create ")}<a href="/">{React.string("links")}</a></li>
  <li>{React.string("...")}</li>
  </ul>
  <p><small>{React.string("Sample content borrowed from ")}<a href="https://markdown-to-jsx.quantizor.dev/">{React.string("markdown-to-jsx")}</a>{React.string(", thanks! ❤️")}</small></p>
  <p>{React.string("Custom handling of code blocks (or any rule!) is possible with the ")}<a href="https://github.com/quantizor/markdown-to-jsx#optionsrenderrule"><code>{React.string("renderRule")}</code>{React.string(" option")}</a>{React.string(". For example, LaTeX support via ")}<a href="https://www.npmjs.com/package/@matejmazur/react-katex"><code>{React.string("@matejmazur/react-katex")}</code></a>{React.string(":")}</p>
  <pre><code class="language-{React.string("latex")}">{React.string("\mathbb{N} = \{ a \in \mathbb{Z} : a &gt; 0 \}")}
  </code></pre>
  <p>{React.string("Or any other typical language, using ")}<a href="https://highlightjs.org/"><code>{React.string("highlight.js")}</code></a>{React.string(":")}</p>
  <pre><code class="language-{React.string("javascript")}">{React.string("function App() {")}
  {React.string("  return &lt;div&gt;Hello world!&lt;/div&gt;;")}
  {React.string("}")}
  </code></pre>
  <p>{React.string("You can even include custom React components if you declare them in the ")}<a href="https://github.com/quantizor/markdown-to-jsx/blob/main/README.md#optionsoverrides---rendering-arbitrary-react-components"><code>{React.string("overrides")}</code>{React.string(" option")}</a>{React.string(".")}</p>
  <p><MyComponent>{React.string("Isn't that cool?")}</MyComponent></p>
  ; </>
        };
      };
      
      Utopia.Router.register((module Page));
