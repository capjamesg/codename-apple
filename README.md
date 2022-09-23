# Codename: Apple

Codename: Apple is a static site generator built in Common Lisp.

Codename: Apple lets you:

- Define HTML templates in Lisp.
- Generate templates for markdown documents based on their front matter.

## Using the Generator

To use Codename: Apple, first load the `generator.lisp` file into your script:

    (load "generator.lisp")

You can now start defining and generating HTML documents in Lisp.

## Defining a HTML element

To define a HTML element, you can use this syntax:

    (tag tagname contents attributes)

Here's an example of a tag that defines a link:

    (princ
        (tag "a" "This is a link!"
        (list
            (attr "href" "example.com")
            (attr "style" "border-bottom: none;"))))

The generator returns this code as a HTML element:

    <a href='example.com' style='border-bottom: none;' >This is a link!</a>

To define more complicated documents, you can nest values:

    (princ
        (tag "div"
            (tag "a" "This is a link!"
                (list
                    (attr "href" "example.com")
                    (attr "style" "border-bottom: none;")))
        (list (attr "class" "h-entry"))))

This nests our `<a>` tag from earlier in a `<div>` with the class name `h-entry`:

    <div class='h-entry' ><a href='example.com' style='border-bottom: none;' >This is a link!</a></div>

## TODOs

[ ] Document how front matter parsing works.
[ ] Add inline comments for all functions.
[ ] Remove functions used for my specific website.

## Dependencies

This project relies on the following Common Lisp dependencies:

- cl-ppcre
- uiop
- markdown

## License

This project is licensed under an [MIT license](LICENSE).

## Contributors

- capjamesg