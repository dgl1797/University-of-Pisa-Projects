import { Response, Request } from "express";
import * as validators from "./validators";
import * as utils from "../../utils";
import errorCodes from "../../configs/errorCodes.json";
import { IUser, Session, User } from "../../models";
import { neo4jdao, redisdao } from "../../daos";

export const getAuth = async (req: Request, res: Response) => {
  let operation = undefined;
  let newUser: User = undefined;
  try {
    // query string validation
    const value = utils.validateSchema(req.query, validators.queryStringSchema);
    // registration logic
    if (value.new === "true") {
      // SIGNUP
      // authorization string validation
      const [username, password, isValidator] = req?.headers?.["authorization"]?.split(":") ?? [
        undefined,
        undefined,
        undefined,
      ];
      utils.validateSchema({ username, password, isValidator }, validators.registrationSchema);
      const { hash, salt } = utils.encryptPassword(password);
      newUser = new User({
        username,
        password: hash,
        salt,
        isValidator: isValidator === "true",
      });
      await newUser.save();
      operation = 1;
      await neo4jdao("WRITE").run(
        `CREATE (u:User{_id: "${newUser._id.toString()}", isValidator: ${
          isValidator === "true" ? 1 : 0
        }})`
      );
      const token = utils.tokenize({
        _id: newUser._id,
        username: newUser.username,
        isValidator: newUser.isValidator,
      });
      const session = new Session(token);
      await session.setExpire(newUser._id.toString(), 21600 /* 6h */);
      return res.status(200).json({ token });
    } else {
      // LOGIN
      const [username, password] = req?.headers["authorization"]?.split(":") ?? [
        undefined,
        undefined,
        undefined,
      ];
      utils.validateSchema({ username, password }, validators.loginSchema);
      const exists: IUser = await User.findOne({ username });
      if (!exists) throw { code: errorCodes.NOT_FOUND, message: `${username} doesn't exist` };
      if (!utils.validatePassword(password, exists.salt, exists.password))
        throw { code: errorCodes.ANAUTHORIZED, message: "invalid credentials" };
      const token = utils.tokenize({
        _id: exists._id,
        username: exists.username,
        isValidator: exists.isValidator,
      });
      const session = new Session(token);
      await session.setExpire(exists._id.toString(), 21600 /* 6h */);
      return res.status(200).json({ token });
    }
  } catch (err) {
    if (operation && newUser) {
      await User.deleteOne({ _id: newUser._id });
    }
    const { code, message } = utils.transformError(err);
    res.status(code).json({ message });
  }
};

export const deleteAuth = async (req: Request, res: Response) => {
  try {
    const token = req?.headers?.["authorization"]?.split(" ")[1];
    const userData: IUser = utils.untokenize(token) as any;
    const session = new Session();
    const redisToken = await session.get(userData._id.toString());
    if (redisToken.token === token) await session.delete(userData._id.toString());
    res.status(200).json({ message: "OK" });
  } catch (error) {
    const { code, message } = utils.transformError(error);
    res.status(code).json({ message });
  }
};
