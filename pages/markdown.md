# This is Markdown

#### You can edit me!

[Markdown](http://daringfireball.net/projects/markdown/) lets you write content in a really natural way.

  * You can have lists, like this one
  * Make things **bold** or *italic*
  * Embed snippets of `code`
  * Create [links](/)
  * ...

<small>Sample content borrowed from [markdown-to-jsx](https://markdown-to-jsx.quantizor.dev/), thanks!</small>

Custom handling of code blocks (or any rule!) is possible with the [`renderRule` option](https://github.com/quantizor/markdown-to-jsx#optionsrenderrule). For example, LaTeX support via [`@matejmazur/react-katex`](https://www.npmjs.com/package/@matejmazur/react-katex):

```latex
\mathbb{N} = \{ a \in \mathbb{Z} : a > 0 \}
```

Or any other typical language, using [`highlight.js`](https://highlightjs.org/):

```javascript
function App() {
  return <div>Hello world!</div>;
}
```

You can even include custom React components if you declare them in the [`overrides` option](https://github.com/quantizor/markdown-to-jsx/blob/main/README.md#optionsoverrides---rendering-arbitrary-react-components).

<MyComponent>Isn't that cool?</MyComponent>
