// DB is a class that has the method: init, findItem, addItem, deleteItem
// that uses the idb library to interact with IndexedDB

import { openDB, deleteDB, wrap, unwrap, DBSchema, IDBPDatabase } from 'idb';


type ItemValue = {
    title: string;
    imageUrl : string | undefined;
    keywords: string[];
};

export interface MyNewDBSchema extends DBSchema {
    item: {
        key: string; // title is the key, which is a string
        value: ItemValue; // The structure of the stored data
        indexes: {
            keywords: string; // The index key type is string (individual keyword)
        };
    };

    category: {
        key: string; // tag is the key, which is a string
        value: any
    }


}


export async function open() {
    console.log("Opening IndexedDB database...");
    const db = await openDB<MyNewDBSchema>('MyDatabase', 1, {
        upgrade(db, oldVersion, newVersion, transaction) {
            if (oldVersion < 1) {
                console.log("Upgrading IndexedDB to version 1");
                const itemObjStore = db.createObjectStore("item", { keyPath: "title"});
                console.log("Object store 'item' created.");

                // add multientry index to item
                itemObjStore.createIndex("keywords", "keywords", { multiEntry: true });
            }
        }
    });

    return new DB(db);
}



class DB {
    // use type annotation for db scema
    // db schema: item has the primary key title (string), and contents (list of strings)


    db : IDBPDatabase <MyNewDBSchema>;

    constructor(db: IDBPDatabase<MyNewDBSchema>) {
        this.db = db;
    }


    async findItem(title: string) {
        return await this.db.get('item', title);
    }

    async listItemKeywords()  : Promise<Record<string, number>> {
        const keywords = await this.db.getAllKeysFromIndex('item', 'keywords');
        const uniqueKeywordsAndCount = keywords.reduce((acc: Record<string, number>, keyword) => {
            acc[keyword] = (acc[keyword] || 0) + 1;
            return acc;
        }, {});
        return uniqueKeywordsAndCount;
    }

    async addItem(item: ItemValue) {
        return await this.db.add('item', item);
    }

    // async deleteItem(id) {
    //     const db = await this.dbPromise;
    //     return db.delete('item', id);
    // }
}
export { DB };