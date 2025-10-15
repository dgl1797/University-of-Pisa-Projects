import Joi from "joi";

export const queryStringSchema = Joi.object({
  new: Joi.string().valid("true", "false").required(),
});

export const registrationSchema = Joi.object({
  username: Joi.string().required(),
  password: Joi.string().required(),
  isValidator: Joi.string().valid("true", "false").required(),
});

export const loginSchema = Joi.object({
  username: Joi.string().required(),
  password: Joi.string().required(),
});
