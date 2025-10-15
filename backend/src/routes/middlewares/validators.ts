import Joi from "joi";
const joi = Joi;

export const bearerSchema = joi.alternatives(
  joi.object({
    bearer: joi.string().valid("Bearer").required(),
    token: joi.string().required(),
  })
);
