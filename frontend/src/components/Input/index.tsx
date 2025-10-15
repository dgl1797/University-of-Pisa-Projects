import React, { HTMLAttributes, HTMLInputTypeAttribute, memo, RefObject, useEffect } from "react";
import classes from "./input.module.css";
import useInput from "./useInput";

type InputProps = {
  inputType: HTMLInputTypeAttribute;
  label: string;
  hasError?: {
    valid: (value: any) => boolean;
    errorMessage: string;
  }[];
  reference: RefObject<HTMLInputElement> | null;
  onError?: () => void;
  focusOnLoad?: boolean;
} & HTMLAttributes<HTMLInputElement>;

const Input = ({
  inputType,
  hasError,
  reference,
  label,
  onError,
  focusOnLoad = false,
  ...props
}: InputProps) => {
  const { validate, handleFocus, isFocused, error, handleDivClick } = useInput(
    reference,
    hasError,
    onError
  );
  useEffect(() => {
    if (focusOnLoad) {
      reference?.current?.focus();
    }
  }, [focusOnLoad, reference]);
  return (
    <div
      className={`${
        !error
          ? !isFocused
            ? classes.inputContainer
            : classes.focusedInputContainer
          : !isFocused
          ? classes.inputErrorContainer
          : classes.focusedInputErrorContainer
      } ${props?.className ?? ""}`}
      onClick={handleDivClick}
    >
      <input
        {...props}
        ref={reference}
        type={inputType}
        className={classes.input}
        onChange={(ev) => {
          if (props.onChange) props.onChange(ev);
          validate();
        }}
        onFocus={() => handleFocus("focus")}
        onBlur={(ev) => {
          handleFocus("blur");
          if (props.onBlur) props.onBlur(ev);
        }}
      />
      <label
        className={`${!isFocused ? classes.inputLabel : classes.focusedInputLabel} ${
          error ? classes.inputLabelError : ""
        }`}
      >
        {label.toUpperCase()}
      </label>
      {error && <label className={classes.errorLabel}>{error}</label>}
    </div>
  );
};

export default memo(Input);
