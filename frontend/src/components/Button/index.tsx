import React, { HTMLAttributes, memo } from "react";
import classes from "./button.module.css";

type ButtonProps = { text: string; disabled?: boolean } & HTMLAttributes<HTMLButtonElement>;

const Button = ({ text, disabled, ...props }: ButtonProps) => {
  return (
    <button
      {...props}
      className={`${classes.buttons} ${props?.className ?? ""}`}
      disabled={disabled}
    >
      {text}
    </button>
  );
};

export default memo(Button);
