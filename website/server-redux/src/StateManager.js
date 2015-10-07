import _ from 'lodash';
import fs from 'fs';
import path from 'path';
import {List, Map, fromJS} from 'immutable';
//import {patch} from 'immutablepatch';
import reducer from './reducer.js';

export default class StateManager {
	static loadInitialState(dataDir) {
		// Load all json files into a map to a list of
		var filename_l = fs.readdirSync(dataDir);
		// The files should be named in order of processing,
		// so sort the array so that we can directly update the item list
		var filename_l = _.filter(filename_l, function(filename) { return path.extname(filename) === ".json" });
		filename_l.sort();

		let state = Map();
		for (let filename of filename_l) {
			const action = JSON.parse(fs.readFileSync(dataDir+filename, "utf8"));
			state = reducer(state, action);
		}
		return state;
	}
}
