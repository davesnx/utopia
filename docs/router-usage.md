# Router Usage

## Overview

Utopia provides a client-side router with support for type-safe routes, partial (diff) navigation, browser history caching, and server-side rendering. The router is built on React Server Components (RSC) and works without a full page reload on navigation.

## Defining Routes

Routes are file-based, following the `app/` directory convention:

```
app/
  layout.re           -> root layout (wraps all pages)
  page.re             -> /
  about/
    page.re           -> /about
    layout.re         -> layout for /about and descendants
    team/
      page.re         -> /about/team
  users/
    [id]/
      page.re         -> /users/:id       (dynamic segment)
  blog/
    [...slug]/
      page.re         -> /blog/*           (catch-all, requires at least one segment)
  docs/
    [[...slug]]/
      page.re         -> /docs or /docs/*  (optional catch-all)
```

Special files:
- `page.re` / `page.mlx` -- page component
- `layout.re` / `layout.mlx` -- layout wrapping all child pages
- `not-found.re` -- custom 404 page (root only)

Route groups `(groupname)` and parallel slots `@name` are invisible in URLs.

## Generated `Routes` Module

The compiler scans the `app/` directory and generates a `Routes` module with typed constructors for every route.

### Static routes

```reason
/* app/page.re -> Routes.route */
let homeRoute = Routes.route;

/* app/about/page.re -> Routes.About.route */
let aboutRoute = Routes.About.route;
```

### Dynamic routes

```reason
/* app/users/[id]/page.re -> Routes.Users.Param_id.make */
let userRoute = Routes.Users.Param_id.make(~id="42", ());
```

### Routes with query and hash schemas

```reason
let searchRoute = Routes.Search.make(
  ~query={term: "hello"},
  ~hash="results",
  (),
);
```

### Pattern matching with `Routes.of_route`

Match a `Utopia.Route.t` back to a typed variant:

```reason
switch (Routes.of_route(router.route)) {
| Some(Routes.Current.Home_page) => "home"
| Some(Routes.Current.Users_param_id({id})) => "user " ++ id
| Some(Routes.Current.About_page) => "about"
| None => "unknown"
};
```

## `Utopia.Route.t`

The core route type is a record with three fields:

| Field | Description | Example |
|-------|-------------|---------|
| `pathname` | Normalized path without query or hash | `/users/42` |
| `request_path` | Pathname + query string | `/users/42?tab=posts` |
| `href` | Full path with query and hash | `/users/42?tab=posts#bio` |

### Constructors

```reason
/* From a raw href string */
let route = Utopia.Route.of_href("/about?ref=nav#top");

/* From explicit parts */
let route = Utopia.Route.make(~pathname="/about", ~query=[("ref", "nav")], ~hash="top", ());

/* From path segments */
let route = Utopia.Route.from_segments(~segments=["users", "42"], ());
```

### Accessors

```reason
Utopia.Route.href(route)           /* "/users/42?tab=posts#bio" */
Utopia.Route.pathname(route)       /* "/users/42" */
Utopia.Route.request_path(route)   /* "/users/42?tab=posts" */
Utopia.Route.path_segments(route)  /* ["users", "42"] */
Utopia.Route.query_entries(route)  /* [("tab", "posts")] */
Utopia.Route.hash(route)           /* Some("bio") */
```

### Comparison

```reason
Utopia.Route.equal(a, b)              /* compares by href */
Utopia.Route.same_pathname(a, b)      /* compares by pathname only */
Utopia.Route.same_request_path(a, b)  /* compares by request_path only */
```

### Params

For dynamic route segments, extracted params are available through `Utopia.Route.Params`:

```reason
Utopia.Route.Params.find_one(params, "id")    /* Some("42") */
Utopia.Route.Params.find_many(params, "slug")  /* Some(["a", "b", "c"]) */
```

## `useRouter` Hook

Access the router from any client component inside `<Utopia />`:

```reason
let router = Utopia.useRouter();
```

Returns a record with:

| Field | Type | Description |
|-------|------|-------------|
| `path` | `string` | Current request path (pathname + query) |
| `route` | `Utopia.Route.t` | Current route |
| `navigate` | see below | Navigate to a new route |

### `router.navigate`

```reason
router.navigate(
  ~history: navigation_history=?,  /* Push (default) or Replace */
  ~freshness: navigation_freshness=?,  /* Use_cache (default) or Revalidate */
  Utopia.Route.t
) => unit
```

**`navigation_history`:**
- `Utopia.Push` -- pushes a new entry onto the browser history stack (default)
- `Utopia.Replace` -- replaces the current history entry

**`navigation_freshness`:**
- `Utopia.Use_cache` -- allows diff (partial) navigation when possible (default)
- `Utopia.Revalidate` -- forces a full re-fetch from the server, bypassing diff and cache

### Examples

```reason
/* Basic navigation (Push + Use_cache) */
router.navigate(Routes.About.route);

/* Replace current entry (e.g., after a redirect) */
router.navigate(~history=Utopia.Replace, Routes.Home.route);

/* Force fresh data after a mutation */
router.navigate(~freshness=Utopia.Revalidate, Routes.Notes.Param_tag.make(~tag, ()));

/* Both: replace + revalidate */
router.navigate(
  ~history=Utopia.Replace,
  ~freshness=Utopia.Revalidate,
  Routes.Notes.Param_tag.make(~tag, ()),
);
```

## `<Utopia.Router.Link>`

A client component that renders an `<a>` tag with client-side navigation on click:

```reason
<Utopia.Router.Link
  to_=Routes.About.route
  ?className
  ~history=Utopia.Push  /* optional, default Push */
>
  {React.string("About")}
</Utopia.Router.Link>
```

Props:

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `to_` | `Utopia.Route.t` | required | Target route |
| `history` | `navigation_history` | `Push` | Push or Replace |
| `className` | `string option` | `None` | CSS class for the anchor |
| `children` | `React.element` | required | Link content |

The component intercepts plain left clicks (no modifier keys) and calls `navigate` instead of triggering a full page load. Modifier clicks (ctrl+click, cmd+click, etc.) fall through to default browser behavior for opening in new tabs.

## `js-route-link` Class (Delegated Link Interception)

As an alternative to `<Utopia.Router.Link>`, you can add the CSS class `js-route-link` to any `<a>` tag to opt into client-side navigation:

```reason
<a href="/about" className="js-route-link">
  {React.string("About")}
</a>
```

The router registers a global click handler on `window` that checks `event.target.closest("a.js-route-link")`. If found and the link is same-origin, the click is intercepted and routed through `navigate`.

This is useful for:
- Server-rendered HTML where you can't use client components
- Third-party content or markdown rendering
- Any `<a>` tag that should navigate without a full reload

Skipped when:
- The event has `defaultPrevented`
- A modifier key is held (meta, ctrl, shift, alt)
- The anchor has `target` set to anything other than `_self`
- The anchor has the `download` attribute

## Layouts

Layout files wrap all pages in their directory and below. They receive a `children` prop:

```reason
/* app/layout.re */
[@react.component]
let make = (~children) =>
  <html>
    <body>
      <nav> /* ... */ </nav>
      children
    </body>
  </html>;
```

Layouts are preserved across navigations within the same subtree. Navigating from `/about/team` to `/about/contact` only re-renders the page content inside the `/about` layout -- the root layout and the `/about` layout stay mounted.

If no layout file exists for a directory level, the router inserts a transparent pass-through layout (`Utopia.PassThroughLayout`) to maintain the nesting structure.

## Server Functions and Revalidation

After calling a server function that mutates data, use `Revalidate` to ensure the page reflects the new state:

```reason
let%browser_only handleSubmit = () => {
  let%await _result = MyServerFunction.call(data);
  router.navigate(~freshness=Utopia.Revalidate, router.route);
};
```

With `~history=Utopia.Replace`, you can revalidate the current page in place without adding a history entry:

```reason
router.navigate(
  ~history=Utopia.Replace,
  ~freshness=Utopia.Revalidate,
  router.route,
);
```
