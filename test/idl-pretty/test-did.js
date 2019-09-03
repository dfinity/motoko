const IDL = require('IDL')
const g = IDL.Obj({'0': IDL.Bool, '1': IDL.Int})
const actor_t = new IDL.ActorInterface({
  'testfun': IDL.Message(IDL.Obj({'0': g}), IDL.Obj({}))})

module.exports = { g, actor_t }
