  $ mkdir app
  $ cat > app/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ utopia build 2>&1
  
  utopia build
  
    ▸ Validating project structure
    ✓ Project structure valid
    ▸ Checking npm dependencies
    ✗ Missing package.json in project root; `utopia build` requires npm dependencies.
      remediation: npm install
  [1]
