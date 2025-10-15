import Joi from "joi";
import errorCodes from "../../configs/errorCodes.json";
const joi = Joi;

export type QueryModel = {
  cloneHops?: boolean; //neo4j (hops)
  likeHops?: boolean; //neo4j query
  averageDuration?: {
    low: number;
    high: number;
  }; //mongo aggregation
  year?: number; //mongo aggregation
  title?: string; //mongo simple filter
  author?: string; //mongo simple filter
  isFilter?: boolean;
  liked?: boolean;
};

const ascDesc = [-1, 1] as const;
type AscDesc = typeof ascDesc[number];

export type SortBy = {
  creationDate: AscDesc; // mongodb
  avgDuration: AscDesc; // mongodb
  likes: AscDesc; //neo4j
};

export const querySchema = joi
  .alternatives(
    joi.object({
      cloneHops: joi.boolean().required(),
    }),
    joi.object({
      likeHops: joi.boolean().required(),
    }),
    joi.object({
      isFilter: joi.boolean().required(),
      averageDuration: joi
        .object({
          low: joi
            .number()
            .required()
            .error(() => {
              throw { code: 400, message: "please, specify a lower bound" };
            }),
          high: joi
            .number()
            .greater(joi.ref("low"))
            .error(() => {
              throw { code: 400, message: "lower bound can not be greater than the upper one" };
            })
            .required()
            .error(() => {
              throw { code: 400, message: "please, specify an upper bound" };
            }),
        })
        .optional(),
      year: joi.number().optional(),
      title: joi.string().optional(),
      author: joi.string().optional(),
      liked: joi.boolean().optional(),
    })
  )
  .error(() => new Error("bad combination of filters"));

export const sortSchema = joi
  .object({
    creationDate: joi.number().valid(1, -1).optional(),
    avgDuration: joi.number().valid(1, -1).optional(),
    likes: joi.number().valid(1, -1).optional(),
  })
  .optional();

export const isPublicSchema = joi.object({
  isMine: joi.boolean().required(),
  isPublic: joi.boolean().optional(),
  username: joi
    .string()
    .required()
    .error(() => {
      throw { code: errorCodes.ANAUTHORIZED, message: "invalid operation without userId" };
    }),
});

export const patchPlaylistSchema = joi.object({
  songsList: joi.array().items(joi.string().required()).required(),
});
