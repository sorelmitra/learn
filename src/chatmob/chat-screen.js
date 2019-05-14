import React from 'react';
import {
	Text, View,
	Image, FlatList, TextInput, Keyboard,
	Dimensions
} from 'react-native';

import { heights, styles } from './styles';
import chatService from './chat-service';

export default class ChatScreen extends React.Component {

	constructor(props) {
		super(props);

		this.state = {
			input: "",
			data: [],
			index: 1,
			messageTypes: [],
			listStyle: { height: this.regularListHeight() }
		}
	}

	componentDidMount() {
		var hardBound = this.keyboardDidShow.bind(this);
		this.keyboardDidShowListener = Keyboard.addListener(
			'keyboardDidShow',
			hardBound,
		);
		hardBound = this.keyboardDidHide.bind(this);
		this.keyboardDidHideListener = Keyboard.addListener(
			'keyboardDidHide',
			hardBound,
		);
	}

	componentDidUpdate() {
	}

	heightsSum() {
		var sum = 0;
		for (var n in heights) {
			sum += heights[n];
		}
		return sum;
	}

	regularListHeight() {
		windowHeight = Dimensions.get('window').height;
		regularHeight = windowHeight - this.heightsSum();
		return regularHeight;
	}

	keyboardDidShow(e) {
		keyboardHeight = e.endCoordinates.height;
		windowHeight = Dimensions.get('window').height;
		shortHeight = windowHeight - keyboardHeight - this.heightsSum();
		console.log("short height: ", shortHeight);
		this.setState({
			listStyle: { height: shortHeight }
		});
		this._conversationView.scrollToEnd();
	}

	keyboardDidHide() {
		this.setState({
			listStyle: { height: this.regularListHeight() }
		});
	}

	addMessage(message, messageType) {
		var o = {
			text: message,
			key: this.state.index.toString()
		};
		console.log("index A <", message, ">", this.state.index);
		this.setState({
			input: "",
			data: [...this.state.data, o],
			index: this.state.index + 1,
			messageTypes: [...this.state.messageTypes, messageType],
		});
		console.log("index B <", message, ">", this.state.index);
	}

	onTextInput(event) {
		this.addMessage(event.nativeEvent.text, "outgoingMessage");
		let self = this;
		chatService.post(event.nativeEvent.text);
		this._conversationView.scrollToEnd();
	}

	textStyle(index) {
		style = styles[this.state.messageTypes[index]];
		return style;
	}

	render() {
		let pic = {
			uri: 'https://cdn.pixabay.com/photo/2019/01/16/20/52/chatbot-3936760_960_720.jpg'
		}
		return (
			<View
				style={this.state.mainViewStyle}
				ref={(c) => this._mainView = c}
			>
				<Image source={pic} style={{ width: '100%', height: heights.logo }} />
				<Text style={styles.heading}>Welcome to BotAgg Chat!</Text>
				<Text style={styles.subtext}>Type your message below</Text>
				<View style={styles.title}><Text>Conversation</Text></View>
				<FlatList
					ref={(c) => this._conversationView = c}
					data={this.state.data}
					style={this.state.listStyle}
					renderItem={({ item, index }) =>
						<View
							style={styles.message}
							accessibilityLabel={item.text}
						>
							<Text
								style={this.textStyle(index)}
							>
								{item.text}
							</Text>
						</View>}
				/>
				<TextInput
					testID="messageText"
					accessibilityLabel="message input field"
					ref={(c) => this._textInput = c}
					value={this.state.input}
					style={styles.input}
					placeholder="Your message goes here"
					onChangeText={(text) => this.setState({ input: text })}
					onSubmitEditing={(event) => this.onTextInput(event)}
				></TextInput>
			</View>
		);
	}
};
