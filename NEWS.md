# ravedash 0.1.2

* Added `start_session` to start a new/existing session with one function
* Added stand-alone viewer support, allowing almost any outputs to be displayed in another session in full-screen, and synchronized with the main session
* Added `register_output` to register outputs, and `get_output_options` to obtain render details. This replaces the shiny output assign and facilitates the stand-alone viewer
* Added `output_gadget` and `output_gadget_container` to display built-in gadgets to outputs, allowing users to download outputs, and to display them in stand-alone viewers; needs little extra setups, must be used with `register_output`

# ravedash 0.1.1

* Added a `NEWS.md` file to track changes to the package.
