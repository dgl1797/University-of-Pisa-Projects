import { RedisBase } from "../daos";

export class Interval extends RedisBase {
  IID: string;
  ["constructor"]: typeof Interval;
  constructor() {
    super();
    this.IID = Date.now().toString();
  }
  static get prefix(): string {
    return "intervals";
  }
}
