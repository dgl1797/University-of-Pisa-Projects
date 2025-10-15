import { RefObject, useCallback, useState } from "react";

const useInput = (
  reference: RefObject<HTMLInputElement> | null,
  hasError?: { valid: (value: any) => boolean; errorMessage: string }[],
  onError?: () => void
) => {
  const [isFocused, setIsFocused] = useState<boolean>(false);
  const [error, setError] = useState<string>("");

  const validate = useCallback(() => {
    if (hasError) {
      if (reference && reference.current) {
        for (let err of hasError) {
          if (err.valid(reference.current.value)) {
            setError("");
          } else {
            setError(err.errorMessage);
          }
        }
      }
    }
  }, [hasError, reference]);

  const handleFocus = useCallback(
    (caller: "focus" | "blur") => {
      if (caller === "blur" && reference?.current?.value) return;
      if (caller === "blur" && error) return;
      setIsFocused(caller === "focus");
    },
    [error, reference]
  );

  const handleDivClick = useCallback(() => {
    if (reference && reference.current) reference.current.focus();
  }, [reference]);
  return { validate, handleFocus, isFocused, error, handleDivClick };
};

export default useInput;
