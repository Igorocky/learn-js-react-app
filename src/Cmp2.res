@react.component
let make = () => {
  <div> {React.string("Hello from rescript.")} </div>
}

React.setDisplayName(make, "Cmp2")