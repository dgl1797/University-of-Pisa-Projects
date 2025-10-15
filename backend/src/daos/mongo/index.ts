import {
  AggregateOptions,
  Filter,
  FindOptions,
  MongoClient,
  MongoClientOptions,
  ObjectId,
  OptionalId,
  UpdateFilter,
} from "mongodb";

export const mongodao = async (
  uri?: string,
  port?: string,
  name?: string,
  options?: MongoClientOptions
) => {
  const dbUri = uri ?? process.env.MONGO_URI;
  const dbPort = port ?? process.env.MONGO_PORT;
  const dbName = name ?? process.env.MONGO_NAME;
  const instance = await MongoClient.connect(`mongodb://${dbUri}:${dbPort}`, options);
  const db = instance.db(dbName);
  return { db, instance };
};

export abstract class MongoBase<T> {
  _id: ObjectId;
  ["constructor"]: typeof MongoBase;
  static get collectionName() {
    return "";
  }
  async save() {
    const { db, instance } = await mongodao();
    let data: any = {};
    Object.assign(data, this);
    const { insertedId } = await db.collection<T>(this.constructor.collectionName).insertOne(data);
    this._id = insertedId;
    await instance.close();
  }
  async update() {
    const { db, instance } = await mongodao();
    let data: any = {};
    Object.assign(data, this);
    await db
      .collection<T>(this.constructor.collectionName)
      .updateOne({ _id: this._id }, { $set: { ...data } });
    await instance.close();
  }
  static async aggregate<T>(pipeline: T[], options?: AggregateOptions) {
    const { db, instance } = await mongodao();
    const result = await db
      .collection<T>(this.collectionName)
      .aggregate(pipeline, options)
      .toArray();
    await instance.close();
    return result;
  }
  static async find<T>(filters: Filter<T>, options?: FindOptions<T>) {
    const { db, instance } = await mongodao();
    const result = await db.collection<T>(this.collectionName).find(filters, options).toArray();
    await instance.close();
    return result;
  }
  static async findOne<T>(filters: Filter<T>) {
    const { db, instance } = await mongodao();
    const result = await db.collection<T>(this.collectionName).findOne(filters);
    await instance.close();
    return result;
  }
  static async deleteOne<T>(filter: Filter<T>) {
    const { db, instance } = await mongodao();
    const result = await db.collection<T>(this.collectionName).deleteOne(filter);
    await instance.close();
    return result;
  }
  static async deleteMany<T>(filter: Filter<T>) {
    const { db, instance } = await mongodao();
    const result = await db.collection<T>(this.collectionName).deleteMany(filter);
    await instance.close();
    return result;
  }
  static async updateOne<T>(filter: Filter<T>, update: UpdateFilter<T> | Partial<T>) {
    const { db, instance } = await mongodao();
    const result = await db.collection<T>(this.collectionName).updateOne(filter, update);
    await instance.close();
    return result;
  }
  static async updateMany<T>(filter: Filter<T>, update: UpdateFilter<T> | Partial<T>) {
    const { db, instance } = await mongodao();
    const result = await db.collection<T>(this.collectionName).updateMany(filter, update);
    await instance.close();
    return result;
  }
  static async insertOne<T>(doc: OptionalId<T>) {
    const { db, instance } = await mongodao();
    const result = await db.collection<T>(this.collectionName).insertOne(doc);
    await instance.close();
    return result;
  }
  static async insertMany<T>(docs: OptionalId<T>[]) {
    const { db, instance } = await mongodao();
    const result = await db.collection<T>(this.collectionName).insertMany(docs);
    await instance.close();
    return result;
  }
}
