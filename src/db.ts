// DB is a class that has the method: init, findItem, addItem, deleteItem
// that uses the idb library to interact with IndexedDB

import { openDB, deleteDB, wrap, unwrap, DBSchema, IDBPDatabase } from 'idb';
import typia, { tags } from 'typia';

type Keyword = string
               & tags.MinLength<1>
               & tags.Pattern<"^[a-zA-Z_:]+$">;

type ItemValue = {
    epc : string & tags.MinLength<24> & tags.MaxLength<24>; // epc is a string of length 24
    title: string & tags.MinLength<1>; // title is a non-empty string
    imageUrl : string | undefined;
    keywords: Keyword[];
};

export interface MyNewDBSchema extends DBSchema {
    item: {
        key: string; // epc is the key, which is a string
        value: ItemValue; // The structure of the stored data
        indexes: {
            byKeyword: string; // The index key type is string (individual keyword)
            byTitle: string; 
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
                const itemObjStore = db.createObjectStore("item", { keyPath: "epc"});
                console.log("Object store 'item' created.");

                // add multientry index to item
                itemObjStore.createIndex("byKeyword", "keywords", { multiEntry: true });
                itemObjStore.createIndex("byTitle", "title");
            }
        }
    });

    return new DB(db);
}



class DB {
    // use type annotation for db scema
    // db schema: item has the primary key epc (string), and contents (list of strings)


    db : IDBPDatabase <MyNewDBSchema>;

    constructor(db: IDBPDatabase<MyNewDBSchema>) {
        this.db = db;
    }


    async getItem(epc: string) {
        return await this.db.get('item', epc);
    }

    async findItemByTitle(title: string) {
        return await this.db.getFromIndex('item', 'byTitle', title);
    }

    async listItemKeywords()  : Promise<Record<string, number>> {
        const keywords = await this.db.getAllKeysFromIndex('item', 'byKeyword');
        const uniqueKeywordsAndCount = keywords.reduce((acc: Record<string, number>, keyword) => {
            acc[keyword] = (acc[keyword] || 0) + 1;
            return acc;
        }, {});
        return uniqueKeywordsAndCount;
    }

    async listItems() : Promise<ItemValue[]> {
        return await this.db.getAll('item');
    }

    async getPartialKeywords(partial: string) : Promise<Record<string, number>> {

        const keyrange = IDBKeyRange.bound(partial, partial + '\uffff');
        const items = await this.db.getAllFromIndex('item', 'byKeyword', keyrange);

        const uniqueKeywordsAndCount = items.reduce((acc: Record<string, number>, item) => {
            item.keywords.forEach((keyword) => {
                if (keyword.startsWith(partial)) {
                    acc[keyword] = (acc[keyword] || 0) + 1;
                }
            });
            return acc;
        }, {});

        return uniqueKeywordsAndCount;
    }

    async addItem(item: ItemValue) {
        typia.assert<ItemValue>(item);
        return await this.db.add('item', item);
    }

    async putItem(item: ItemValue) {
        typia.assert<ItemValue>(item);
        return await this.db.put('item', item);
    }

    async deleteItem(epc: string) {
        return await this.db.delete('item', epc);
    }

    async deleteDB() {
        console.log("Deleting IndexedDB database...");
        await this.db.close();
        await deleteDB('MyDatabase');
        console.log("IndexedDB database deleted.");
    }

    // async deleteItem(id) {
    //     const db = await this.dbPromise;
    //     return db.delete('item', id);
    // }
}
export { DB };