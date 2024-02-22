module Page = {
  let path = "blog";

  /* SSG

     let getInitialProps = () => {
       let payments = Stripe.get_all_products();
       payments
     };
     */

  /* let loader = () => {
       let%lwt payments = Stripe.get_all_products();
       payments
     }; */

  [@react.component]
  let make = () => {
    <div> {React.string("Blog")} </div>;
  };
};

Utopia.Loader.push((module Page));
