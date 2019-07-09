import React from 'react';
import {
	Text, View,
	Image, FlatList, TextInput, Keyboard,
	StyleSheet, Animated
} from 'react-native';
import Config from 'react-native-config';

var ChatService = require('./utils/ChatService').ChatService;
import postNotifService from './postnotif-service';

export default class ChatScreen extends React.Component {

	constructor(props) {
		super(props);

		this.state = {
			input: "",
			data: [],
			index: 1,
			styles: [],
			messagePostingStatuses: [],
		}

		this.chatService = new ChatService(fetch, Config.CHAT_NAME, Config.CHAT_POSTS_URL);
		this.expandedPicHeight = 150;
		this.minimizedPicHeight = 80;
		this.keyboardHeight = new Animated.Value(0);
		this.picHeight = new Animated.Value(this.expandedPicHeight);
	}

	componentDidMount() {
		this.keyboardWillShowListener = Keyboard.addListener(
			'keyboardWillShow',
			this.keyboardWillShow.bind(this),
		);
		this.keyboardWillHideListener = Keyboard.addListener(
			'keyboardWillHide',
			this.keyboardWillHide.bind(this),
		);
		this.keyboardDidShowListener = Keyboard.addListener(
			'keyboardDidShow',
			this.keyboardDidShow.bind(this),
		);
		this.keyboardDidHideListener = Keyboard.addListener(
			'keyboardDidHide',
			this.keyboardDidHide.bind(this),
		);

		postNotifService.register(Config.CHAT_NOTIFICATIONS_URL);
		this.incomingMessageListener = postNotifService.addListener(
			this.messageIn.bind(this)
		)
	}

	keyboardWillShow(e) {
		let showDuration = e.duration * 0.60;
		Animated.parallel([
			Animated.timing(this.keyboardHeight, {
				duration: showDuration,
				toValue: e.endCoordinates.height
			}),
			Animated.timing(this.picHeight, {
				duration: showDuration,
				toValue: this.minimizedPicHeight
			})
		]).start();
	}

	keyboardDidShow(e) {
	}

	keyboardWillHide(e) {
		Animated.parallel([
			Animated.timing(this.keyboardHeight, {
				duration: e.duration,
				toValue: 0
			}),
			Animated.timing(this.picHeight, {
				duration: e.duration,
				toValue: this.expandedPicHeight
			})
		]).start();
	}

	keyboardDidHide(e) {
		//this._conversationView.scrollToEnd({animated: false});
	}

	addMessage(message, status, style) {
		var o = {
			text: message,
			key: this.state.index.toString()
		};
		this.setState({
			input: "",
			data: [o, ...this.state.data],
			index: this.state.index + 1,
			styles: [style, ...this.state.styles],
			messagePostingStatuses: [status, ...this.state.messagePostingStatuses],
		});
	}

	onTextInput(event) {
		let message = event.nativeEvent.text;
		let self = this;
		this.chatService.post(message)
		.then(function(resp) {
			console.log(resp);
			self.addMessage(message, `${self.timestamp()} >`,
				{
					row: "styleMessageRow",
					messageBox: "styleMessageBoxOut",
					messageText: "styleOutgoingMessage",
					messageStatus: "styleMessageStatusOut",
				});
		})
		.catch(function(error) {
			console.log(`Server refused posting message <${message}> with response: ${error}`);
			self.addMessage(message, `${self.timestamp()} !`,
				{
					row: "styleMessageRow",
					messageBox: "styleMessageBoxOut",
					messageText: "styleOutgoingMessage",
					messageStatus: "styleMessageStatusOutErr",
				});
		});
		this.onConversationChanged();
	}

	messageIn(post) {
		this.addMessage(post.body, `< ${this.timestamp()}`,
			{
				row: "styleMessageRowReverse",
				messageBox: "styleMessageBoxIn",
				messageText: "styleIncomingMessage",
				messageStatus: "styleMessageStatusIn",
			});
	}

	showLastMessage(options = {flash: true}) {
		this._conversationView.scrollToOffset({x:0, y: 0, animated: false});
		if (options == null) {
			return;
		}
		if (options.flash == true) {
			this._conversationView.flashScrollIndicators();
		}
	}

	onConversationChanged(contentWidth, contentHeight) {
		this.showLastMessage();
	}

	timestamp() {
		let d = new Date(Date.now());
		let month = d.toLocaleString('en-us', { month: 'short' });
		let date = d.getDate();
		let h = d.getHours().toString().padStart(2, "0");
		let m = d.getMinutes().toString().padStart(2, "0");
		let s = d.getSeconds().toString().padStart(2, "0");
		return `${month} ${date} ${h}:${m}:${s}`;
	}

	rowStyle(index) {
		return styles[this.state.styles[index].row];
	}

	messageBoxStyle(index) {
		return styles[this.state.styles[index].messageBox];
	}

	textStyle(index) {
		return styles[this.state.styles[index].messageText];
	}

	messageStatusStyle(index) {
		return styles[this.state.styles[index].messageStatus];
	}

	messageStatus(index) {
		return this.state.messagePostingStatuses[index];
	}

	getMessageStatusTestId(index) {
		return `messageStatus-${index}`;
	}

	getMessageTestId(index) {
		return `message-${index}`;
	}

	render() {
		let pic = {
			uri: 'https://cdn.pixabay.com/photo/2019/01/16/20/52/chatbot-3936760_960_720.jpg'
		}
		return (
			<View
				style={styles.styleMainView}
				ref={(c) => this._mainView = c}
			>
				<Animated.Image source={pic} style={{ flex: 0, width: '100%', height: this.picHeight }} />
				<View><Text style={[styles.styleTitle, {flex: 0}]}>Conversation</Text></View>
				<Animated.View style={{flex: 1, paddingBottom: this.keyboardHeight}}>
					<View 
						style={[styles.styleContainer, {flex: 1}]} 
					>
						<FlatList
							testID="conversationList"
							ref={(c) => this._conversationView = c}
							data={this.state.data}
							style={styles.styleList}
							inverted={true}
							renderItem={({ item, index }) =>
								<View style={this.rowStyle(index)}>
									<View style={this.messageBoxStyle(index)}>
										<Text 
											style={this.messageStatusStyle(index)}
											testID={this.getMessageStatusTestId(index)}
										>
											{this.messageStatus(index)}
										</Text>
										<Text
											accessibilityLabel={item.text}
											style={this.textStyle(index)}
											testID={this.getMessageTestId(index)}
										>
											{item.text}
										</Text>
									</View>
								</View>
							}
						/>
					</View>
					<View style={[styles.styleContainer, {flex: 0}]}>
						<TextInput
							testID="messageText"
							accessibilityLabel="message input field"
							ref={(c) => this._textInput = c}
							value={this.state.input}
							style={styles.styleInput}
							blurOnSubmit={false}
							placeholder="Type your message"
							onChangeText={(text) => this.setState({ input: text })}
							onSubmitEditing={(event) => this.onTextInput(event)}
						></TextInput>
					</View>
				</Animated.View>
			</View>
		);
	}
};


const styles = StyleSheet.create({
	styleMainView: {
		backgroundColor: '#E0E0E0',
		flex: 1
	},
	styleContainer: {
		alignItems: 'stretch',
		justifyContent: 'flex-start',
		backgroundColor: '#FFFFFF',
		margin: 5
	},
	styleTitle: {
		height: 16,
		color: 'black',
		fontSize: 14,
		alignItems: 'center',
		justifyContent: 'flex-end',
		borderBottomWidth: 1,
		margin: 5
	},
	styleMessageRow: {
		flexDirection: 'row'
	},
	styleMessageRowReverse: {
		flexDirection: 'row-reverse'
	},
	styleMessageBoxIn: {
		backgroundColor: '#E0FFE0',
		borderWidth: StyleSheet.hairlineWidth,
		borderRadius: 5,
		width: 140,
		flexDirection: "column",
		alignItems: "flex-end"
	},
	styleMessageBoxOut: {
		backgroundColor: '#FFF0FF',
		borderWidth: StyleSheet.hairlineWidth,
		borderRadius: 5,
		width: 140,
		flexDirection: "column",
		alignItems: "flex-start"
	},
	styleMessageStatusIn: {
		alignSelf: "flex-start",
		fontSize: 10,
		color: '#303030',
	},
	styleMessageStatusOut: {
		alignSelf: "flex-end",
		fontSize: 10,
		color: '#303030',
	},
	styleMessageStatusOutErr: {
		alignSelf: "flex-end",
		fontSize: 12,
		color: '#FF0000',
	},
	styleList: { 
		margin: 5, 
		alignSelf: 'stretch' 
	},
	styleInput: {
		height: 24,
		borderWidth: StyleSheet.hairlineWidth,
		borderRadius: 5,
		padding: 3
	},
	styleOutgoingMessage: {
		color: 'green',
		fontSize: 14
	},
	styleIncomingMessage: {
		color: 'red',
		fontSize: 14,
		textAlign: 'right'
	}
});

