"use strict";

import {Dialog} from "@mui/material";

const make = ({opn, children}) => {
    return <Dialog open={opn}>{children}</Dialog>
}

export default make