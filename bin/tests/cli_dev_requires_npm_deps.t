  $ mkdir app
  $ cat > app/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ utopia dev --no-watch 2>&1
  
  utopia dev
  
    ▸ Running initial build bootstrap
    ▸ Checking npm dependencies
    ✗ Missing package.json in project root; `utopia dev` requires npm dependencies.
      remediation: npm install
  [1]
