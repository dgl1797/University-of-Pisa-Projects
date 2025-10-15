import { NextFunction, Request, Response } from "express";
import { IUser, Session, User } from "../../models";
import { delay, transformError, untokenize, validateSchema } from "../../utils";
import * as validators from "./validators";
import errorCodes from "../../configs/errorCodes.json";
import { redisdao } from "../../daos";

export const validateAuthorization = async (req: Request, res: Response, next: NextFunction) => {
  try {
    // method Bearer <token>
    const [bearer, token] = req?.headers?.["authorization"]?.split(" ");
    validateSchema({ bearer, token }, validators.bearerSchema);
    const userData: IUser = untokenize(token) as any;
    const exists: IUser = await User.findOne({ username: userData.username });
    if (!exists)
      throw { code: errorCodes.NOT_FOUND, message: `${userData.username} does not exist` };
    if (!exists._id.equals(userData._id))
      throw { code: errorCodes.ANAUTHORIZED, message: "this id is not authorized here" };
    if (!(exists.isValidator === userData.isValidator))
      throw {
        code: errorCodes.ANAUTHORIZED,
        message: "Level of authorization is not matching",
      };
    const session = new Session();
    const userToken = await session.get(userData._id.toString());
    if (userToken.token !== token) throw { code: errorCodes.FORBIDDEN, message: "invalid session" };
    next();
  } catch (error) {
    const { code, message } = transformError(error);
    res.status(code).json({ message });
  }
};
