var React = require('react');
var AppDispatcher = require('./AppDispatcher');


module.exports = React.createClass(
    {
        gotIt: function()
        {
            console.log("a");
            AppDispatcher.dispatch({operation:"got_item",
                                    data:this.props.item});
            console.log("b");
        },        
        render: function()
        {
            return (<tr>
                    <td>{this.props.item.name}</td>
                    <td className="button_column"><button onClick={this.gotIt}>Got it!</button></td>
                    </tr>);
        }
    });
