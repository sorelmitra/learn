var notif = require('./../chatmob/utils/ChatApiNotif');

var n = new notif.ChatApiNotif("hi");
n.receiveRegistrationResponse("{\"reason\": \"fake\"}")