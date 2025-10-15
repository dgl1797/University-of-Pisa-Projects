import crypto from "crypto";
import Joi from "joi";
import jwt from "jsonwebtoken";
import errorCodes from "../configs/errorCodes.json";
import { mongodao, redisdao } from "../daos";
import { Session } from "../models";
import { Request, Response } from "express";
import { ObjectId } from "mongodb";
import moment from "moment";

export const delay = (millis: number) => new Promise((resolve) => setTimeout(resolve, millis));

/** function that matches a plain-text and a couple hash+salt
 * @param password the plain-text received as password
 * @param salt the salt field inside the users' collection
 * @param hash the hashed password retrieved by the users' collection
 * @returns a boolean that checks if the hashed password is equals to the encrypted plain-text
 */
export const validatePassword = (password, salt, hash) => {
  return hash === crypto.pbkdf2Sync(password, salt, 10, 64, "sha512").toString("hex");
};

/** function to validate a joi schema
 * @param data the data to be validated
 * @param schema the joi schema to use as validation model
 * @param code eventual code to return as error in case of validation failure
 * @returns a value object containing the validated values
 */
export const validateSchema = <T>(
  data: T,
  schema: Joi.Schema<T>,
  code: number = errorCodes.BAD_REQUEST
) => {
  const { error, value } = schema.validate(data);
  if (error) throw { code, message: error.message };
  return value;
};

/** function to encrypt a plain-text
 * @param password the plain-text to be encrypted
 * @returns the generated hashed password and the salt generated randomly
 */
export const encryptPassword = (password) => {
  const salt = crypto.randomBytes(32).toString("hex");
  const hash = crypto.pbkdf2Sync(password, salt, 10, 64, "sha512").toString("hex");
  return {
    hash,
    salt,
  };
};

/** function to tokenize some data
 * @param data the object to be tokenized
 * @param expiresIn expiration time in milliseconds or as a string "10h", "7d": https://github.com/vercel/ms
 * @param secret the secret to generate the token, default taken by a JWT_SECRET variable in environment
 * @param options other options for the token
 * @returns the token generated
 */
export const tokenize = (
  data: string | object | Buffer,
  expiresIn?: string | number,
  secret?: jwt.Secret,
  options?: jwt.SignOptions
) => {
  const jwtSecret: jwt.Secret = !!secret ? secret : process?.env?.JWT_SECRET ?? undefined;
  if (!jwtSecret) throw { code: errorCodes.INTERNAL, message: "invalid secret key" };
  const jwtOptions = options;
  if (expiresIn) jwtOptions.expiresIn = expiresIn;
  return jwt.sign(data, jwtSecret, jwtOptions);
};

/** function that verifies and decodes a token
 * @param token token to be decoded and verified
 * @param secret secret to use for the verification
 * @param verifyOptions verification options
 * @param decodeOptions decoding options
 * @returns the decoded data
 */
export const untokenize = (
  token: string,
  secret?: jwt.Secret,
  verifyOptions?: jwt.VerifyOptions & {
    complete: true;
  },
  decodeOptions?: jwt.DecodeOptions & {
    complete: true;
  }
) => {
  const jwtSecret = !!secret ? secret : process?.env?.JWT_SECRET ?? undefined;
  if (!jwtSecret) throw { code: errorCodes.INTERNAL, message: "invalid secret key" };
  if (!jwt.verify(token, jwtSecret, verifyOptions))
    throw { code: errorCodes.ANAUTHORIZED, message: "invalid token" };
  return jwt.decode(token, decodeOptions);
};

/** function to handle repetitive actions on errors and logging it
 * @param error catched error to be handled
 * @param options.defaultCode code to display if the catched error doesn't have a code
 * @param options.defaultMessage message to display if the catched error doesn't have a message
 * @param options.duplicateCode code to display if the code is 11000 (mongo's duplicate key exception)
 * @param options.duplicateMessage message to display if the code is 11000 (mongo's duplicate key exception)
 * @returns an error object containing a code and a message
 */
export const transformError = (
  error,
  options?: {
    defaultCode: number;
    defaultMessage: string;
    duplicateCode: number;
    duplicateMessage: string;
  }
): { code: number; message: string } => {
  console.error(error);
  if ((error?.code ?? errorCodes.INTERNAL) === 11000) {
    error.code = options?.duplicateCode ?? errorCodes.FORBIDDEN;
    error.message = options?.duplicateMessage ?? "Duplicate Not Allowed";
  }
  if ((error?.code ?? errorCodes.INTERNAL) === "ServiceUnavailable") {
    error.code = errorCodes.INTERNAL;
    error.message = "Failed to connect to Neo4j";
  }
  if (typeof (error?.code ?? "") === "string") {
    error.code = errorCodes.INTERNAL;
    error.message = "Internal Error";
  }
  if (!error.code) error.code = options?.defaultCode ?? errorCodes.INTERNAL;
  if (!error.message) error.message = options?.defaultMessage ?? "Internal Error";
  return error;
};

export const getAppInfos = async (req: Request, res: Response) => {
  try {
    const { db, instance } = await mongodao();
    const activeSessions = await redisdao().keys(`${Session.prefix}:*`);
    const activeIds = activeSessions.map((session) => session.split(":")[1]);
    const activeValidators = await db
      .collection("users")
      .find({ _id: { $in: activeIds.map((aid) => new ObjectId(aid)) }, isValidator: true })
      .count();
    const numberOfSongs = await db.collection("songs").countDocuments();
    const numberOfPublishedPlaylist = await db
      .collection("playlists")
      .find({ isPublic: true })
      .count();
    const numberOfPlaylistsCreatedThisYear =
      (
        await db
          .collection("playlists")
          .aggregate([
            {
              $project: {
                creationYear: { $year: "$creationDate" },
              },
            },
            {
              $match: {
                creationYear: moment().year(),
              },
            },
            {
              $count: "number",
            },
          ])
          .toArray()
      )?.[0]?.number ?? undefined;
    return res.status(200).json({
      reply: {
        activeUsers: activeSessions.length,
        activeValidators,
        numberOfSongs,
        numberOfPublishedPlaylist,
        numberOfPlaylistsCreatedThisYear,
      },
    });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};
