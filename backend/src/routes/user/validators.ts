import Joi from "joi";
const joi = Joi;

export const updateSchema = joi
  .object({
    username: joi.string().min(1).required(),
  })
  .required();
