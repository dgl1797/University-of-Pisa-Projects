import React, { HTMLAttributes, memo, PropsWithChildren } from "react";
import classes from "./card.module.css";

type CardProps = {
  title?: string;
  titleClassName?: string;
} & HTMLAttributes<HTMLDivElement>;

const Card = ({ title, titleClassName, children, ...props }: PropsWithChildren<CardProps>) => {
  return (
    <div className={`${classes.card} ${props?.className ?? ""}`} onClick={props.onClick}>
      <div className={`${classes.title} ${titleClassName ?? ""}`}>{title}</div>
      {children}
    </div>
  );
};

export default memo(Card);
