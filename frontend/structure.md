# Views:

- ## Folder with the view name in CapitalCamelCase

  - an index.tsx file containing the rendering and the memo(component) as default export.
  - an [componentName].module.css file for defining local css classes and styles.
  - an use[componentName].tsx exporting an hook with all the component's logics and the styles classes.
  - if necessary, a file other_components.tsx exporting components to be used inside index.tsx only

- ## index.js exporter file
  - import every main component in this file and export all of them in a unique object

# Store:

- ## Folder with the reducers
  - ### Folder with the slice in snake_case
    - a selectors.tsx file exporting all the selector functions necessary to access the values of the slice.
    - an index.tsx file importing the selectors from the ./selectors.tsx file as selectors and exporting them, and
      the slice definition and exportation
    - an interface.tsx file containing all the types and the interfaces necessary inside the index.tsx file
  - ### an index.js exporter file
    - import everything with import \* as [sliceName] from ./slice_name and export the reducer inside of the
      reducer object; the selectors inside the selectors object and the actions inside the actions object
- ## index.ts file containing the definition of the store

# Hooks:

- ## Folder of the hook in camelCase
  - file index.tsx containing the custom hook exported as default
- ## index.js exporter file
  - import the hooks and export them in an unique object as in views folder

# Contexts:

- ## If necessary structured as the Hooks folder
